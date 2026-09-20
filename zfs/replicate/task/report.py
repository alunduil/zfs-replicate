"""Task Reporting Functions."""

import itertools
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Generic, TypeVar

from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .type import (
    CreateFilesystemTask,
    DestroyFilesystemTask,
    DestroySnapshotTask,
    SendSnapshotTask,
    Task,
)

Key = TypeVar("Key")


class Action(Enum):
    """What a task does, coarse enough to group the two destroys together."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


def _action(task: Task) -> Action:
    match task:
        case CreateFilesystemTask():
            return Action.CREATE
        case SendSnapshotTask():
            return Action.SEND
        case DestroyFilesystemTask() | DestroySnapshotTask():
            return Action.DESTROY


def _snapshot(task: Task) -> Snapshot | None:
    match task:
        case SendSnapshotTask() | DestroySnapshotTask():
            return task.snapshot
        case _:
            return None


@dataclass(frozen=True)
class _Level(Generic[Key]):
    """One grouping level of the report.

    ``limit`` is how many buckets this level spells out before falling back
    to counts.  ``after`` is the level those counts descend into.
    """

    name: str
    limit: int
    key: Callable[[Task], Key]
    after: "_Level[Any] | None" = None


_SNAPSHOTS = _Level(name="snapshot", limit=13, key=_snapshot)
_ACTIONS = _Level(name="action", limit=4, key=_action, after=_SNAPSHOTS)
_FILESYSTEMS = _Level(name="filesystem", limit=6, key=lambda task: task.filesystem, after=_ACTIONS)


def report(tasks: list[Task]) -> str:
    """Pretty printed report on given Tasks."""
    filesystems = [(filesystem, list(tasks)) for filesystem, tasks in itertools.groupby(tasks, key=_FILESYSTEMS.key)]

    if len(filesystems) > _FILESYSTEMS.limit:
        return _counts(_FILESYSTEMS, tasks)

    return _report_filesystem(filesystems)


def _report_filesystem(filesystems: list[tuple[FileSystem, list[Task]]]) -> str:
    output = ""

    for filesystem, tasks in filesystems:
        output += f"\nfilesystem: {filesystem.name}\n"

        actions = [(action, list(tasks)) for action, tasks in itertools.groupby(tasks, key=_ACTIONS.key)]

        if len(actions) > _ACTIONS.limit:
            output += _counts(_ACTIONS, tasks, indentation="    ")
        else:
            output += _report_action(actions, indentation=" - ")

    return output


def _report_action(actions: list[tuple[Action, list[Task]]], indentation: str = "") -> str:
    output = ""

    for action, tasks in actions:
        output += f"{indentation}action: {action}\n"

        snapshots = [
            (snapshot, list(tasks))
            for snapshot, tasks in itertools.groupby(tasks, key=_SNAPSHOTS.key)
            if snapshot is not None
        ]

        if len(snapshots) > _SNAPSHOTS.limit:
            output += _counts(_SNAPSHOTS, tasks, indentation="   " + indentation)
        else:
            output += _report_snapshot(snapshots, indentation="   " + indentation)

    return output


def _report_snapshot(snapshots: list[tuple[Snapshot, list[Task]]], indentation: str = "") -> str:
    output = "\n".join([f"{indentation}snapshot: {s}" for s, _ in snapshots])

    if output:
        output += "\n"

    return output


def _counts(level: _Level[Any], tasks: list[Task], indentation: str = "") -> str:
    group = {level.key(x) for x in tasks}

    output = f"{indentation}{level.name}:{len(group)}\n"

    if level.after is not None:
        output += _counts(level.after, tasks, indentation=indentation)

    return output
