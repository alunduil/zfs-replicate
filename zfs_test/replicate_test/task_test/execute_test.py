"""zfs.replicate.task.execute tests."""

import logging
from typing import Any, List

import pytest

from zfs.replicate import filesystem, receive, send, snapshot
from zfs.replicate.command import Command
from zfs.replicate.compress import Compression
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.type import FileSystem
from zfs.replicate.filesystem.type import filesystem as filesystem_t
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.execute import execute
from zfs.replicate.task.type import Action, Task

REMOTE = filesystem_t("backup")
SEND_OPTIONS = send.Options(large_block=False, raw=True, embed=False, compressed=False, props=False)
RECEIVE_OPTIONS = receive.Options(force=True, no_mount=False, resume=False, properties={})
SSH_COMMAND = Command("ssh", ["backup.example.com"])


def test_send_dispatch_logs(caplog: pytest.LogCaptureFixture, monkeypatch: pytest.MonkeyPatch) -> None:
    """Dispatching a SEND task logs the snapshot at INFO."""

    def fake_send(*_args: Any, **_kwargs: Any) -> None:
        return None

    monkeypatch.setattr(snapshot, "send", fake_send)
    # click_log.basic_config disables propagation on zfs.replicate, so caplog
    # (which captures via the root logger) sees nothing without this.
    monkeypatch.setattr(logging.getLogger("zfs.replicate"), "propagate", True)

    local = filesystem_t("tank/data")
    snap = Snapshot(filesystem=local, name="snap1", previous=None, timestamp=0)
    task = Task(action=Action.SEND, filesystem=local, snapshot=snap)

    with caplog.at_level(logging.INFO, logger="zfs.replicate"):
        _execute([task])

    # Assert on the snapshot identity, not the exact phrasing, so rewording the
    # progress message doesn't fail this.
    dispatch = [r for r in caplog.records if r.levelno == logging.INFO]
    assert any("tank/data@snap1" in r.getMessage() for r in dispatch)


def test_actions_route_to_their_operations(monkeypatch: pytest.MonkeyPatch) -> None:
    """CREATE, snapshot DESTROY, and filesystem DESTROY reach distinct calls."""
    calls: List[str] = []

    def fake_create(target: FileSystem, **_kwargs: Any) -> None:
        calls.append(f"create filesystem {target.name}")

    def fake_destroy_filesystem(target: FileSystem, **_kwargs: Any) -> None:
        calls.append(f"destroy filesystem {target.name}")

    def fake_destroy_snapshot(target: Snapshot, **_kwargs: Any) -> None:
        calls.append(f"destroy snapshot {target.filesystem.name}@{target.name}")

    monkeypatch.setattr(filesystem, "create", fake_create)
    monkeypatch.setattr(filesystem, "destroy", fake_destroy_filesystem)
    monkeypatch.setattr(snapshot, "destroy", fake_destroy_snapshot)

    stale = filesystem_t("backup/tank/stale")
    _execute(
        [
            Task(action=Action.CREATE, filesystem=filesystem_t("backup/tank/fresh"), snapshot=None),
            Task(
                action=Action.DESTROY,
                filesystem=stale,
                snapshot=Snapshot(filesystem=stale, name="snap1", previous=None, timestamp=0),
            ),
            Task(action=Action.DESTROY, filesystem=stale, snapshot=None),
        ]
    )

    assert calls == [
        "create filesystem backup/tank/fresh",
        "destroy snapshot backup/tank/stale@snap1",
        "destroy filesystem backup/tank/stale",
    ]


def test_failed_data_set_raises_once_the_others_finish(monkeypatch: pytest.MonkeyPatch) -> None:
    """One unreachable data set doesn't stop the rest, and the run still errors."""
    sent = []

    def fake_send(_remote: Any, current: Snapshot, **_kwargs: Any) -> None:
        if current.filesystem.name == "tank/broken":
            raise ZFSReplicateError("no route to host")

        sent.append(current.filesystem.name)

    monkeypatch.setattr(snapshot, "send", fake_send)

    tasks = [_send_task("tank/broken"), _send_task("tank/intact")]

    with pytest.raises(ZFSReplicateError):
        _execute(tasks, jobs=2)

    assert sent == ["tank/intact"]


def _execute(tasks: List[Task], jobs: int = 1) -> None:
    execute(
        REMOTE,
        tasks,
        ssh_command=SSH_COMMAND,
        compression=Compression.LZ4,
        send_options=SEND_OPTIONS,
        receive_options=RECEIVE_OPTIONS,
        jobs=jobs,
    )


def _send_task(name: str) -> Task:
    return Task(
        action=Action.SEND,
        filesystem=REMOTE,
        snapshot=Snapshot(filesystem=filesystem_t(name), name="snap1", previous=None, timestamp=0),
    )
