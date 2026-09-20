"""Task Execution."""

from ..filesystem import FileSystem
from .type import Replication, Task


def execute(tasks: list[tuple[FileSystem, list[Task]]], replication: Replication) -> None:
    """Execute all tasks, deepest filesystem first."""
    sorted_tasks = sorted(tasks, key=lambda x: len(x[0].name.split("/")), reverse=True)

    for _, filesystem_tasks in sorted_tasks:
        for task in filesystem_tasks:
            task.run(replication)
