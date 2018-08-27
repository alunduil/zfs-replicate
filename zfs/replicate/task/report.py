"""Task Reporting Functions."""

import itertools
from typing import Dict, Iterator, Optional, Sequence

from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .type import Action, Task

LIMITS = {"filesystem": 3, "action": 4, "snapshot": 12}

AFTERS = {"filesystem": "action", "action": "snapshot"}


def report(tasks: Sequence[Task]) -> str:
    """Pretty printed report on given Tasks."""

    groupable, countable = itertools.tee(tasks, 2)

    filesystems = dict(itertools.groupby(groupable, key=lambda x: x.filesystem))

    if len(filesystems) > LIMITS["filesystem"]:
        return _counts("filesystem", countable)

    return _report_dataset(filesystems)


def _report_dataset(filesystems: Dict[FileSystem, Iterator[Task]]) -> str:
    output = ""

    for filesystem, tasks in filesystems.items():
        output += f"dataset: {filesystem}\n"

        groupable, countable = itertools.tee(tasks, 2)

        actions = dict(itertools.groupby(groupable, key=_action))

        if len(actions) > LIMITS["action"]:
            output += _counts("action", countable, indentation="    ")
        else:
            output += _report_action(actions, indentation=" - ")

    return output


def _report_action(actions: Dict[Action, Iterator[Task]], indentation: str = "") -> str:
    output = ""

    for action, tasks in actions.items():
        output += f"{indentation}action: {action}\n"

        groupable, countable = itertools.tee(tasks, 2)

        snapshots = dict(itertools.groupby(groupable, key=lambda x: x.snapshot))

        if len(snapshots) > LIMITS["snapshot"]:
            output += _counts("snapshot", countable, indentation=indentation + "   ")
        else:
            output += _report_snapshot(snapshots, indentation="   " + indentation)

    return output


def _report_snapshot(snapshots: Dict[Optional[Snapshot], Iterator[Task]], indentation: str = "") -> str:
    return "\n".join([f"{indentation}snapshot: {s.name}" for s in snapshots if s is not None])


def _counts(current: str, tasks: Iterator[Task], indentation: str = "") -> str:
    group = {getattr(x, current) for x in tasks}

    output = f"{indentation}{current}:{len(group)}\n"

    if current in AFTERS:
        output += _counts(AFTERS[current], tasks, indentation=indentation)

    return output


# Excising this makes typing happy.  Check if it can be injected.
def _action(task: Task) -> Action:
    return task.action
