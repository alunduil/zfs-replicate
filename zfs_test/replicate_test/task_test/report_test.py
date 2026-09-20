"""zfs.replicate.task.report tests."""

from hypothesis import given
from hypothesis.strategies import builds, lists, one_of

from zfs.replicate.task import report
from zfs.replicate.task.type import (
    CreateFilesystemTask,
    DestroyFilesystemTask,
    DestroySnapshotTask,
    SendSnapshotTask,
    Task,
)

TASKS = one_of(
    builds(CreateFilesystemTask),
    builds(SendSnapshotTask),
    builds(DestroyFilesystemTask),
    builds(DestroySnapshotTask),
)


class TestReport:
    """A report is empty exactly when there are no tasks."""

    def test_empty_tasks(self) -> None:
        """Ensure no actions is an empty report."""
        assert report([]) == ""

    @given(tasks=lists(TASKS, min_size=1))
    def test_nonempty_tasks(self, tasks: list[Task]) -> None:
        """Ensure nonempty report from nonempty actions."""
        result = report(tasks)
        assert result != ""
