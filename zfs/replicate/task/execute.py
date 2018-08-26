"""Task Execution."""

import itertools

from typing import Dict, List, Optional

from .. import dataset

from ..dataset import DataSet
from .. import snapshot
from .type import Action, Task

def execute(tasks: Dict[DataSet, List[Task]], ssh_command: Optional[str()]=None):
    """Execute all tasks."""

    for _, dataset_tasks in sorted(tasks.items, key=lambda x: len(x[0].split("/")), reverse=True):
        for action, action_tasks in itertools.groupby(dataset_tasks, key=lambda x: x.action):
            if action == Action.CREATE:
                _create(action_tasks, ssh_command=ssh_command)
            elif action == Action.DESTROY:
                _destroy(action_tasks, ssh_command=ssh_command)
            elif action == Action.SEND:
                _send(action_tasks, ssh_command=ssh_command)


def _create(tasks: List[Task], ssh_command: Optional[str()]=None):
    for task in tasks:
        dataset.create(task.dataset, ssh_command=ssh_command)


def _destroy(tasks: List[Task], ssh_command: Optional[str()]=None):
    for task in tasks:
        if task.snapshot is None:
            dataset.destroy(task.dataset, ssh_command=ssh_command)
        else:
            snapshot.destroy(task.snapshot, ssh_command=ssh_command)


def _send(tasks: List[Task], ssh_command: Optional[str()]=None):
    if tasks:
        snapshot.send(task[0].snapshot, previous=None, ssh_command=ssh_command)

        for task, previous in zip(tasks[1:], tasks):
            snapshot.send(task.snapshot, previous=previous, ssh_command=ssh_command)
