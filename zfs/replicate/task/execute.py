"""Task Execution."""

import itertools
from typing import List, Tuple

from .. import filesystem, optional, snapshot
from ..compress import Compression
from ..filesystem import FileSystem
from .type import Action, Task


def execute(  # pylint: disable=too-many-arguments
    remote: FileSystem,
    tasks: List[Tuple[FileSystem, List[Task]]],
    ssh_command: str,
    follow_delete: bool,
    compression: Compression,
) -> None:
    """Execute all tasks."""

    sorted_tasks = sorted(tasks, key=lambda x: len(x[0].name.split("/")), reverse=True)

    for _, filesystem_tasks in sorted_tasks:
        action_tasks = {
            action: list(action_tasks)
            for action, action_tasks in itertools.groupby(filesystem_tasks, key=lambda x: x.action)
        }

        for action, a_tasks in action_tasks.items():
            if action == Action.CREATE:
                _create(a_tasks, ssh_command=ssh_command)
            elif action == Action.DESTROY:
                _destroy(a_tasks, ssh_command=ssh_command)
            elif action == Action.SEND:
                _send(remote, a_tasks, ssh_command=ssh_command, follow_delete=follow_delete, compression=compression)


def _create(tasks: List[Task], ssh_command: str) -> None:
    for task in tasks:
        filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy(tasks: List[Task], ssh_command: str) -> None:
    for task in tasks:
        if task.snapshot is None:
            filesystem.destroy(task.filesystem, ssh_command=ssh_command)
        else:
            snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(
    remote: FileSystem, tasks: List[Task], ssh_command: str, follow_delete: bool, compression: Compression
) -> None:
    for task in tasks:
        snapshot.send(
            remote,
            optional.value(task.snapshot),
            ssh_command=ssh_command,
            compression=compression,
            follow_delete=follow_delete,
            previous=optional.value(task.snapshot).previous,
        )
