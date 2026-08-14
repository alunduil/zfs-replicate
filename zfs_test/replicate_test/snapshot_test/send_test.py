"""zfs.replicate.snapshot.send tests."""

from typing import List, Optional, Tuple

import pytest

from zfs.replicate import process
from zfs.replicate.command import Command
from zfs.replicate.compress.type import Compression
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options as ReceiveOptions
from zfs.replicate.send.type import Options
from zfs.replicate.snapshot.send import Pipeline, _pipeline, _send, send
from zfs.replicate.snapshot.type import Snapshot

REMOTE = filesystem("backup")
SSH = Command.with_empty_env("ssh", "host")
RECEIVE_DEFAULTS = ReceiveOptions()


def _snapshot(name: str = "pool/data", snapshot: str = "snap") -> Snapshot:
    return Snapshot(
        filesystem=filesystem(name),
        name=snapshot,
        previous=None,
        timestamp=0,
    )


def _assemble(
    *,
    compression: Compression = Compression.OFF,
    receive_options: ReceiveOptions = RECEIVE_DEFAULTS,
    previous: Optional[Snapshot] = None,
) -> Pipeline:
    return _pipeline(
        REMOTE,
        _snapshot(),
        ssh_command=SSH,
        compression=compression,
        send_options=Options(),
        receive_options=receive_options,
        previous=previous,
    )


class _FakeStream:
    """Stand-in for a piped stdout, recording whether the parent closed it."""

    def __init__(self) -> None:
        self.closed = False

    def close(self) -> None:
        """Close the stream, as the real ``IO[bytes]`` would."""
        self.closed = True


class _FakeProcess:
    """Stand-in for the processes the replication pipeline spawns."""

    def __init__(self, command: Command, returncode: int, error: bytes) -> None:
        self.command = command
        self.returncode = returncode
        self.stdout = _FakeStream()
        self._error = error

    def communicate(self) -> Tuple[bytes, bytes]:
        """Return the captured streams the real ``Popen`` would."""
        return (b"", self._error)


def _capture_spawns(monkeypatch: pytest.MonkeyPatch, returncode: int = 0, error: bytes = b"") -> List[_FakeProcess]:
    """Replace the process boundary and collect the processes it hands back."""
    spawned: List[_FakeProcess] = []

    def fake_open(command: Command, **_kwargs: object) -> _FakeProcess:
        spawned.append(_FakeProcess(command, returncode, error))

        return spawned[-1]

    monkeypatch.setattr(process, "open", fake_open)

    return spawned


def _replicate(compression: Compression = Compression.OFF) -> None:
    send(
        REMOTE,
        _snapshot(),
        ssh_command=SSH,
        compression=compression,
        send_options=Options(),
        receive_options=RECEIVE_DEFAULTS,
    )


def test_send_renders_enabled_flags() -> None:
    """_send embeds the -X flag for each enabled send option."""
    command = _send(_snapshot(), options=Options(large_block=True, props=True))

    assert "-L" in command.args
    assert "-p" in command.args


def test_send_omits_disabled_flags() -> None:
    """_send leaves out the -X flag for each disabled send option."""
    command = _send(_snapshot(), options=Options(raw=False))

    assert "-w" not in command.args
    assert "-L" not in command.args


def test_send_keeps_hostile_snapshot_as_one_token() -> None:
    """A snapshot name with shell metacharacters stays a single argv token."""
    command = _send(_snapshot("pool/data a$b"), options=Options())

    assert command.args[-1] == "pool/data a$b@snap"
    assert "'pool/data a$b@snap'" in command.render()


def test_pipeline_drops_both_compression_stages_when_off() -> None:
    """OFF leaves no local compressor and no remote decompress prefix."""
    pipeline = _assemble(compression=Compression.OFF)

    assert pipeline.send.args[-1] == "pool/data@snap"
    assert pipeline.compress is None
    assert pipeline.receive.args[-1] == "/usr/bin/env - zfs receive -F -d backup/pool"


def test_pipeline_pairs_the_compressor_with_a_remote_decompress() -> None:
    """lz4 compresses locally and decompresses ahead of the remote receive."""
    pipeline = _assemble(compression=Compression.LZ4)

    assert pipeline.compress is not None
    assert pipeline.compress.argv == ["/usr/bin/env", "-", "lz4"]
    assert pipeline.receive.args[-1] == "/usr/bin/env - lz4 -d | /usr/bin/env - zfs receive -F -d backup/pool"


def test_pipeline_hands_the_remote_side_to_ssh() -> None:
    """The remote pipeline rides as a single argument to the configured ssh command."""
    pipeline = _assemble()

    assert pipeline.receive.argv[:4] == ["/usr/bin/env", "-", "ssh", "host"]
    assert len(pipeline.receive.argv) == 5


def test_pipeline_marks_an_incremental_send_with_its_previous_snapshot() -> None:
    """A previous snapshot becomes the -i source of the send stage."""
    pipeline = _assemble(previous=_snapshot(snapshot="older"))

    assert pipeline.send.args[-3:] == ["-i", "pool/data@older", "pool/data@snap"]


def test_pipeline_omits_the_incremental_flag_for_a_full_send() -> None:
    """Without a previous snapshot the send stage carries no -i source."""
    pipeline = _assemble()

    assert "-i" not in pipeline.send.args


def test_pipeline_threads_receive_options_into_the_remote_side() -> None:
    """Non-default receive settings reach the remote zfs receive invocation."""
    pipeline = _assemble(
        receive_options=ReceiveOptions(force=False, no_mount=True, resume=True, properties={"readonly": "on"}),
    )

    assert pipeline.receive.args[-1] == "/usr/bin/env - zfs receive -u -s -o readonly=on -d backup/pool"


def test_send_spawns_each_assembled_stage(monkeypatch: pytest.MonkeyPatch) -> None:
    """Each assembled stage runs as a process, in pipeline order."""
    spawned = _capture_spawns(monkeypatch)

    _replicate(compression=Compression.LZ4)

    assert [proc.command.args[1] for proc in spawned] == ["zfs", "lz4", "ssh"]


def test_send_detaches_the_parent_from_every_upstream_stage(monkeypatch: pytest.MonkeyPatch) -> None:
    """The parent closes every piped stdout but the last, whose output it reads."""
    spawned = _capture_spawns(monkeypatch)

    _replicate(compression=Compression.LZ4)

    assert [proc.stdout.closed for proc in spawned] == [True, True, False]


def test_send_ignores_a_missing_mountpoint(monkeypatch: pytest.MonkeyPatch) -> None:
    """Replication tolerates a failure to create the destination mountpoint."""
    _capture_spawns(monkeypatch, returncode=1, error=b"cannot mount 'backup/pool': failed to create mountpoint")

    _replicate()


def test_send_raises_on_any_other_failure(monkeypatch: pytest.MonkeyPatch) -> None:
    """A failed pipeline surfaces its stderr as a ZFSReplicateError."""
    _capture_spawns(monkeypatch, returncode=1, error=b"cannot receive: dataset does not exist")

    with pytest.raises(ZFSReplicateError, match="pool/data@snap"):
        _replicate()
