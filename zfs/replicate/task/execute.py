"""Task Execution."""

import itertools
from typing import Dict, Iterator, List

from .. import filesystem, optional, snapshot
from ..compress import Compression
from ..filesystem import FileSystem
from .type import Action, Task


def execute(
    tasks: Dict[FileSystem, Iterator[Task]], ssh_command: str, follow_delete: bool, compression: Compression
) -> None:
    """Execute all tasks."""

    sorted_items = sorted(tasks.items(), key=lambda x: len(x[0].name.split("/")), reverse=True)

    for _, filesystem_tasks in sorted_items:
        for action, action_tasks in itertools.groupby(filesystem_tasks, key=lambda x: x.action):
            if action == Action.CREATE:
                _create(action_tasks, ssh_command=ssh_command)
            elif action == Action.DESTROY:
                _destroy(action_tasks, ssh_command=ssh_command)
            elif action == Action.SEND:
                _send(list(action_tasks), ssh_command=ssh_command, follow_delete=follow_delete, compression=compression)


def _create(tasks: Iterator[Task], ssh_command: str) -> None:
    for task in tasks:
        filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy(tasks: Iterator[Task], ssh_command: str) -> None:
    for task in tasks:
        if task.snapshot is None:
            filesystem.destroy(task.filesystem, ssh_command=ssh_command)
        else:
            snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(tasks: List[Task], ssh_command: str, follow_delete: bool, compression: Compression) -> None:
    if tasks:
        snapshot.send(
            optional.value(tasks[0].snapshot),
            ssh_command=ssh_command,
            compression=compression,
            follow_delete=follow_delete,
            previous=None,
        )

        for task, previous in zip(tasks[1:], tasks):
            snapshot.send(
                optional.value(task.snapshot),
                ssh_command=ssh_command,
                compression=compression,
                follow_delete=follow_delete,
                previous=previous.snapshot,
            )
