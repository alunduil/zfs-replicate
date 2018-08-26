"""Task Reporting Functions."""

import itertools
from typing import Dict, Sequence

from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .type import Action, Task

LIMITS = {"filesystem": 3, "action": 4, "snapshot": 12}

AFTERS = {"filesystem": "action", "action": "snapshot"}


def report(tasks: Sequence[Task]) -> str():
    """Pretty printed report on given Tasks."""

    filesystems = itertools.groupby(tasks, key=lambda x: x.filesystem)

    if len(filesystems) > LIMITS["filesystem"]:
        return _counts("filesystem", tasks)

    return _report_dataset(filesystems)


def _report_dataset(filesystems: Dict[FileSystem, Sequence[Task]]) -> str():
    output = ""

    for filesystem, tasks in filesystems:
        output += f"dataset: {filesystem}\n"

        actions = itertools.groupby(tasks, key=lambda x: x.action)

        if len(actions) > LIMITS["action"]:
            output += _counts("action", tasks, indentation="    ")
        else:
            output += _report_action(actions, indentation=" - ")

    return output


def _report_action(actions: Dict[Action, Sequence[Task]], indentation: str() = "") -> str():
    output = ""

    for action, tasks in actions:
        output += f"{indentation}action: {action}\n"

        snapshots = itertools.groupby(tasks, key=lambda x: x.snapshot)

        if len(snapshots) > LIMITS["snapshot"]:
            output += _counts("snapshot", tasks, indentation=indentation + "   ")
        else:
            output += _report_snapshot(snapshots, indentation="   " + indentation)

    return output


def _report_snapshot(snapshots: Dict[Snapshot, Sequence[Task]], indentation: str() = "") -> str():
    output = ""

    for snapshot in snapshots:
        output += f"{indentation}snapshot: {snapshot.name}\n"

    return output


def _counts(current: str(), tasks: Sequence[Task], indentation: str() = "") -> str():
    group = {getattr(x, current) for x in tasks}

    output = f"{indentation}{current}:{len(group)}\n"

    if current in AFTERS:
        output += _counts(AFTERS[current], tasks, indentation=indentation)

    return output
