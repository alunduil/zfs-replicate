"""Task Execution."""

import itertools

from .context import RunContext
from .type import Task


def execute(tasks: list[Task], context: RunContext) -> None:
    """Execute all tasks."""
    for task in _deepest_first(tasks):
        task.run(context)


def _deepest_first(tasks: list[Task]) -> list[Task]:
    """Tasks grouped by filesystem, deepest first.

    zfs destroy is recursive, so taking a parent first would remove the
    children while their own tasks are still queued.
    """
    by_filesystem = [(fs, list(grouped)) for fs, grouped in itertools.groupby(tasks, key=lambda x: x.filesystem)]

    return [
        task
        for _, filesystem_tasks in sorted(by_filesystem, key=lambda pair: pair[0].depth, reverse=True)
        for task in filesystem_tasks
    ]
