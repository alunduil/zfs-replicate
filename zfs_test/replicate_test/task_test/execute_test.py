"""zfs.replicate.task.execute tests."""

import logging
from contextlib import ExitStack
from typing import get_args
from unittest import mock

import pytest
from hypothesis import example, given
from hypothesis.strategies import lists
from pytest_mock import MockerFixture

from zfs.replicate import snapshot
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.execute import execute
from zfs.replicate.task.type import CreateFilesystem, DestroySnapshot, SendSnapshot, Task
from zfs_test.replicate_test.task_test.context import CONTEXT
from zfs_test.replicate_test.task_test.strategies import LOCAL, TASKS


class TestExecute:
    """Every task runs once, in order within its filesystem, and logs its dispatch."""

    @given(lists(TASKS))
    @example(
        [
            DestroySnapshot(
                filesystem=LOCAL, snapshot=Snapshot(filesystem=LOCAL, name="s1", previous=None, timestamp=0)
            ),
            CreateFilesystem(filesystem=LOCAL),
            DestroySnapshot(
                filesystem=LOCAL, snapshot=Snapshot(filesystem=LOCAL, name="s2", previous=None, timestamp=1)
            ),
        ]
    )
    def test_runs_every_task_in_order(self, tasks: list[Task]) -> None:
        """Runs one filesystem's tasks as given, even when an action recurs after another; see #653."""
        ran: list[Task] = []
        with ExitStack() as stack:
            for kind in get_args(Task):
                stack.enter_context(
                    mock.patch.object(kind, "run", autospec=True, side_effect=lambda task, _: ran.append(task))
                )

            execute(
                tasks,
                CONTEXT,
            )

        assert ran == tasks

    def test_send_dispatch_logs(
        self,
        caplog: pytest.LogCaptureFixture,
        mocker: MockerFixture,
    ) -> None:
        """Dispatching a SendSnapshot logs the snapshot at INFO."""
        mocker.patch.object(snapshot, "send")
        # click_log.basic_config disables propagation on zfs.replicate, so caplog
        # (which captures via the root logger) sees nothing without this.
        mocker.patch.object(logging.getLogger("zfs.replicate"), "propagate", True)

        local = filesystem("tank/data")
        snap = Snapshot(filesystem=local, name="snap1", previous=None, timestamp=0)
        task = SendSnapshot(filesystem=local, snapshot=snap)

        with caplog.at_level(logging.INFO, logger="zfs.replicate"):
            execute(
                [task],
                CONTEXT,
            )

        # Assert on the snapshot identity, not the exact phrasing, so rewording the
        # progress message doesn't fail this.
        dispatch = [r for r in caplog.records if r.levelno == logging.INFO]
        assert any("tank/data@snap1" in r.getMessage() for r in dispatch)
