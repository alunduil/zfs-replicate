"""Types for Tasks."""

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import ClassVar

from .. import filesystem as filesystem_ops
from .. import snapshot as snapshot_ops
from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .context import RunContext

logger = logging.getLogger(__name__)


class Action(Enum):
    """Task Action.

    A grouping key for the report; dispatch goes through the task types.
    """

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


@dataclass(frozen=True)
class _BaseTask(ABC):
    """A unit of replication work."""

    filesystem: FileSystem
    action: ClassVar[Action]

    @abstractmethod
    def run(self, context: RunContext) -> None:
        """Perform the task against the remote."""


@dataclass(frozen=True)
class CreateFilesystemTask(_BaseTask):
    """A filesystem to create on the remote."""

    action: ClassVar[Action] = Action.CREATE

    def run(self, context: RunContext) -> None:
        """Create the filesystem."""
        logger.info("creating filesystem %s", self.filesystem.name)
        filesystem_ops.create(self.filesystem, ssh_command=context.ssh_command)


@dataclass(frozen=True)
class SendSnapshotTask(_BaseTask):
    """A snapshot to send to the remote."""

    snapshot: Snapshot
    action: ClassVar[Action] = Action.SEND

    def run(self, context: RunContext) -> None:
        """Send the snapshot, incremental from its predecessor when it has one."""
        logger.info("sending snapshot %s", self.snapshot)
        snapshot_ops.send(
            context.remote,
            self.snapshot,
            ssh_command=context.ssh_command,
            compression=context.compression,
            send_options=context.send_options,
            receive_options=context.receive_options,
            previous=self.snapshot.previous,
        )
        logger.debug("sent snapshot %s", self.snapshot)


@dataclass(frozen=True)
class DestroyFilesystemTask(_BaseTask):
    """A filesystem to destroy on the remote."""

    action: ClassVar[Action] = Action.DESTROY

    def run(self, context: RunContext) -> None:
        """Destroy the filesystem."""
        logger.info("destroying filesystem %s", self.filesystem.name)
        filesystem_ops.destroy(self.filesystem, ssh_command=context.ssh_command)


@dataclass(frozen=True)
class DestroySnapshotTask(_BaseTask):
    """A snapshot to destroy on the remote."""

    snapshot: Snapshot
    action: ClassVar[Action] = Action.DESTROY

    def run(self, context: RunContext) -> None:
        """Destroy the snapshot."""
        logger.info("destroying snapshot %s", self.snapshot)
        snapshot_ops.destroy(self.snapshot, ssh_command=context.ssh_command)


# Signatures take Task, never _BaseTask: only the union lets mypy reject
# reading a snapshot off a task that has none.
Task = CreateFilesystemTask | SendSnapshotTask | DestroyFilesystemTask | DestroySnapshotTask
