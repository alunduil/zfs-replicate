"""Task Execution."""

import itertools

from .context import RunContext
from .type import Task


def execute(tasks: list[Task], context: RunContext) -> None:
    """Execute all tasks, deepest filesystem first."""
    by_filesystem = [(fs, list(grouped)) for fs, grouped in itertools.groupby(tasks, key=lambda x: x.filesystem)]

    for _, filesystem_tasks in sorted(by_filesystem, key=lambda x: len(x[0].name.split("/")), reverse=True):
        for task in filesystem_tasks:
            task.run(context)
