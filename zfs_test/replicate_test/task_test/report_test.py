"""zfs.replicate.task.report tests."""

from typing import List

from hypothesis import given
from hypothesis.strategies import builds, lists

from zfs.replicate.task import report
from zfs.replicate.task.type import Task


def test_empty_tasks() -> None:
    """Ensure no actions is an empty report."""
    assert "" == report([])  # nosec


@given(tasks=lists(builds(Task), min_size=1))  # type: ignore[misc]
def test_nonempty_tasks(tasks: List[Task]) -> None:
    """Ensure nonempty report from nonempty actions."""
    result = report(tasks)
    assert "" != result  # nosec
