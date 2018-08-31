"""Task Reporting Functions."""

import itertools
from typing import Dict, List, Optional

from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .type import Action, Task

LIMITS = {"filesystem": 3, "action": 4, "snapshot": 13}

AFTERS = {"filesystem": "action", "action": "snapshot"}


def report(tasks: List[Task]) -> str:
    """Pretty printed report on given Tasks."""

    filesystems = {
        filesystem: list(tasks) for filesystem, tasks in itertools.groupby(tasks, key=lambda x: x.filesystem)
    }

    if len(filesystems) > LIMITS["filesystem"]:
        return _counts("filesystem", tasks)

    return _report_dataset(filesystems)


def _report_dataset(filesystems: Dict[FileSystem, List[Task]]) -> str:
    output = ""

    for filesystem, tasks in filesystems.items():
        output += f"dataset: {filesystem.name}\n"

        actions = {action: list(tasks) for action, tasks in itertools.groupby(tasks, key=_action)}

        if len(actions) > LIMITS["action"]:
            output += _counts("action", tasks, indentation="    ")
        else:
            output += _report_action(actions, indentation=" - ")

    return output


def _report_action(actions: Dict[Action, List[Task]], indentation: str = "") -> str:
    output = ""

    for action, tasks in actions.items():
        output += f"{indentation}action: {action}\n"

        snapshots = {snapshot: list(tasks) for snapshot, tasks in itertools.groupby(tasks, key=lambda x: x.snapshot)}

        if len(snapshots) > LIMITS["snapshot"]:
            output += _counts("snapshot", tasks, indentation="   " + indentation)
        else:
            output += _report_snapshot(snapshots, indentation="   " + indentation)

    return output


def _report_snapshot(snapshots: Dict[Optional[Snapshot], List[Task]], indentation: str = "") -> str:
    return "\n".join([f"{indentation}snapshot: {s.filesystem.name}@{s.name}" for s in snapshots if s is not None])


def _counts(current: str, tasks: List[Task], indentation: str = "") -> str:
    group = {getattr(x, current) for x in tasks}

    output = f"{indentation}{current}:{len(group)}\n"

    if current in AFTERS:
        output += _counts(AFTERS[current], tasks, indentation=indentation)

    return output


# Excising this makes typing happy.  Check if it can be injected.
def _action(task: Task) -> Action:
    return task.action
