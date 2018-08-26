"""Task Execution."""

import itertools
from typing import Dict, List, Optional

from .. import filesystem, snapshot
from ..filesystem import FileSystem
from .type import Action, Task


def execute(tasks: Dict[FileSystem, List[Task]], ssh_command: Optional[str()]=None):
    """Execute all tasks."""

    sorted_items = sorted(tasks.items, key=lambda x: len(x[0].split("/")), reverse=True)

    for _, filesystem_tasks in sorted_items:
        for action, action_tasks in itertools.groupby(filesystem_tasks, key=lambda x: x.action):
            if action == Action.CREATE:
                _create(action_tasks, ssh_command=ssh_command)
            elif action == Action.DESTROY:
                _destroy(action_tasks, ssh_command=ssh_command)
            elif action == Action.SEND:
                _send(action_tasks, ssh_command=ssh_command)


def _create(tasks: List[Task], ssh_command: Optional[str()]=None):
    for task in tasks:
        filesystem.create(task.filesystem, ssh_command=ssh_command)


def _destroy(tasks: List[Task], ssh_command: Optional[str()]=None):
    for task in tasks:
        if task.snapshot is None:
            filesystem.destroy(task.filesystem, ssh_command=ssh_command)
        else:
            snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(tasks: List[Task], ssh_command: Optional[str()]=None):
    if tasks:
        snapshot.send(tasks[0].snapshot, previous=None, ssh_command=ssh_command)

        for task, previous in zip(tasks[1:], tasks):
            snapshot.send(task.snapshot, previous=previous, ssh_command=ssh_command)
