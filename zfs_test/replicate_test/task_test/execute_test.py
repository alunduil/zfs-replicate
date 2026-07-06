"""zfs.replicate.task.execute tests."""

import logging
from typing import Any

import pytest

from zfs.replicate import receive, send, snapshot
from zfs.replicate.compress import Compression
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.execute import execute
from zfs.replicate.task.type import Action, Task


def test_send_dispatch_logs(
    caplog: pytest.LogCaptureFixture, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Dispatching a SEND task logs the snapshot at INFO."""

    def fake_send(*_args: Any, **_kwargs: Any) -> None:
        return None

    monkeypatch.setattr(snapshot, "send", fake_send)

    local = filesystem("tank/data")
    snap = Snapshot(filesystem=local, name="snap1", previous=None, timestamp=0)
    task = Task(action=Action.SEND, filesystem=local, snapshot=snap)

    with caplog.at_level(logging.INFO, logger="zfs.replicate"):
        execute(
            filesystem("backup"),
            [(local, [task])],
            ssh_command="ssh backup.example.com",
            compression=Compression.LZ4,
            send_options=send.Options(
                large_block=False, raw=True, embed=False, compressed=False, props=False
            ),
            receive_options=receive.Options(
                force=True, no_mount=False, resume=False, properties={}
            ),
        )

    assert "sending snapshot tank/data@snap1" in caplog.text  # nosec
