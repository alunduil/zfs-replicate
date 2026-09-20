"""Types for Tasks."""

from dataclasses import dataclass
from enum import Enum, auto

from ..filesystem import FileSystem
from ..snapshot import Snapshot


class Action(Enum):
    """Task Action."""

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


@dataclass(frozen=True)
class CreateFilesystemTask:
    """Create a filesystem on the remote."""

    filesystem: FileSystem

    @property
    def action(self) -> Action:
        """Action this Task performs."""
        return Action.CREATE


@dataclass(frozen=True)
class SendSnapshotTask:
    """Send a snapshot to the remote."""

    filesystem: FileSystem
    snapshot: Snapshot

    @property
    def action(self) -> Action:
        """Action this Task performs."""
        return Action.SEND


@dataclass(frozen=True)
class DestroyFilesystemTask:
    """Destroy a filesystem on the remote."""

    filesystem: FileSystem

    @property
    def action(self) -> Action:
        """Action this Task performs."""
        return Action.DESTROY


@dataclass(frozen=True)
class DestroySnapshotTask:
    """Destroy a snapshot on the remote."""

    filesystem: FileSystem
    snapshot: Snapshot

    @property
    def action(self) -> Action:
        """Action this Task performs."""
        return Action.DESTROY


# A snapshot belongs to some tasks and not others, so each shape gets its own
# dataclass.  Reading one off a task that has none is then a type error rather
# than a None to unwrap at runtime.
Task = CreateFilesystemTask | SendSnapshotTask | DestroyFilesystemTask | DestroySnapshotTask
