"""zfs.replicate.snapshot.send tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.send.type import Options
from zfs.replicate.snapshot.send import _send
from zfs.replicate.snapshot.type import Snapshot


def _snapshot(name: str = "pool/data") -> Snapshot:
    return Snapshot(
        filesystem=filesystem(name),
        name="snap",
        previous=None,
        timestamp=0,
    )


def test_send_renders_enabled_flags() -> None:
    """_send embeds the -X flag for each enabled send option."""
    command = _send(_snapshot(), options=Options(large_block=True, props=True))

    assert "-L" in command.args  # nosec
    assert "-p" in command.args  # nosec


def test_send_omits_disabled_flags() -> None:
    """_send leaves out the -X flag for each disabled send option."""
    command = _send(_snapshot(), options=Options(raw=False))

    assert "-w" not in command.args  # nosec
    assert "-L" not in command.args  # nosec


def test_send_keeps_hostile_snapshot_as_one_token() -> None:
    """A snapshot name with shell metacharacters stays a single argv token."""
    command = _send(_snapshot("pool/data a$b"), options=Options())

    assert command.args[-1] == "pool/data a$b@snap"  # nosec
    assert "'pool/data a$b@snap'" in command.render()  # nosec
