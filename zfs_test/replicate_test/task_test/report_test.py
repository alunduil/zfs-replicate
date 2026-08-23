"""zfs.replicate.task.report tests."""

from typing import List

from hypothesis import given
from hypothesis.strategies import builds, lists

from zfs.replicate.task import report
from zfs.replicate.task.type import Task


class TestReport:
    """A report is empty exactly when there are no tasks."""

    def test_empty_tasks(self) -> None:
        """Ensure no actions is an empty report."""
        assert report([]) == ""

    @given(tasks=lists(builds(Task), min_size=1))
    def test_nonempty_tasks(self, tasks: List[Task]) -> None:
        """Ensure nonempty report from nonempty actions."""
        result = report(tasks)
        assert result != ""
