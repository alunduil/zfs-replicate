"""Types for Tasks."""

from dataclasses import dataclass
from enum import Enum, auto
from typing import ClassVar

from ..filesystem import FileSystem
from ..snapshot import Snapshot


class Action(Enum):
    """Task Action.

    A grouping key for the report; dispatch goes through the task types.
    """

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


@dataclass(frozen=True)
class CreateFilesystemTask:
    """Create a filesystem on the remote."""

    filesystem: FileSystem
    action: ClassVar[Action] = Action.CREATE


@dataclass(frozen=True)
class SendSnapshotTask:
    """Send a snapshot to the remote."""

    filesystem: FileSystem
    snapshot: Snapshot
    action: ClassVar[Action] = Action.SEND


@dataclass(frozen=True)
class DestroyFilesystemTask:
    """Destroy a filesystem on the remote."""

    filesystem: FileSystem
    action: ClassVar[Action] = Action.DESTROY


@dataclass(frozen=True)
class DestroySnapshotTask:
    """Destroy a snapshot on the remote."""

    filesystem: FileSystem
    snapshot: Snapshot
    action: ClassVar[Action] = Action.DESTROY


# Separate types let mypy reject reading a snapshot off a task that has none.
Task = CreateFilesystemTask | SendSnapshotTask | DestroyFilesystemTask | DestroySnapshotTask
