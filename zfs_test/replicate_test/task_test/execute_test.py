"""zfs.replicate.task.execute tests."""

import logging

import pytest
from pytest_mock import MockerFixture

from zfs.replicate import receive, send, snapshot
from zfs.replicate.command import Command
from zfs.replicate.compress import Compression
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.execute import execute
from zfs.replicate.task.type import Action, Task


class TestExecute:
    """Dispatching a task reports it through the ``zfs.replicate`` logger."""

    def test_send_dispatch_logs(
        self,
        caplog: pytest.LogCaptureFixture,
        mocker: MockerFixture,
    ) -> None:
        """Dispatching a SEND task logs the snapshot at INFO."""
        mocker.patch.object(snapshot, "send")
        # click_log.basic_config disables propagation on zfs.replicate, so caplog
        # (which captures via the root logger) sees nothing without this.
        mocker.patch.object(logging.getLogger("zfs.replicate"), "propagate", True)

        local = filesystem("tank/data")
        snap = Snapshot(filesystem=local, name="snap1", previous=None, timestamp=0)
        task = Task(action=Action.SEND, filesystem=local, snapshot=snap)

        with caplog.at_level(logging.INFO, logger="zfs.replicate"):
            execute(
                filesystem("backup"),
                [(local, [task])],
                ssh_command=Command("ssh", ["backup.example.com"]),
                compression=Compression.LZ4,
                send_options=send.Options(large_block=False, raw=True, embed=False, compressed=False, props=False),
                receive_options=receive.Options(force=True, no_mount=False, resume=False, properties={}),
            )

        # Assert on the snapshot identity, not the exact phrasing, so rewording the
        # progress message doesn't fail this.
        dispatch = [r for r in caplog.records if r.levelno == logging.INFO]
        assert any("tank/data@snap1" in r.getMessage() for r in dispatch)
