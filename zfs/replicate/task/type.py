"""Types for Tasks."""

import logging
from dataclasses import dataclass
from enum import Enum, auto
from typing import ClassVar

from .. import filesystem as filesystem_ops
from .. import receive, send
from .. import snapshot as snapshot_ops
from ..command import Command
from ..compress import Compression
from ..filesystem import FileSystem
from ..snapshot import Snapshot

logger = logging.getLogger(__name__)


class Action(Enum):
    """Task Action.

    A grouping key for the report; dispatch goes through the task types.
    """

    CREATE = auto()
    DESTROY = auto()
    SEND = auto()


@dataclass(frozen=True)
class Replication:
    """What every task needs beyond itself to reach the remote."""

    remote: FileSystem
    ssh_command: Command
    compression: Compression
    send_options: send.Options
    receive_options: receive.Options


@dataclass(frozen=True)
class CreateFilesystemTask:
    """Create a filesystem on the remote."""

    filesystem: FileSystem
    action: ClassVar[Action] = Action.CREATE

    def run(self, replication: Replication) -> None:
        """Create the filesystem."""
        logger.info("creating filesystem %s", self.filesystem.name)
        filesystem_ops.create(self.filesystem, ssh_command=replication.ssh_command)


@dataclass(frozen=True)
class SendSnapshotTask:
    """Send a snapshot to the remote."""

    filesystem: FileSystem
    snapshot: Snapshot
    action: ClassVar[Action] = Action.SEND

    def run(self, replication: Replication) -> None:
        """Send the snapshot, resuming from its predecessor."""
        logger.info("sending snapshot %s", self.snapshot)
        snapshot_ops.send(
            replication.remote,
            self.snapshot,
            ssh_command=replication.ssh_command,
            compression=replication.compression,
            send_options=replication.send_options,
            receive_options=replication.receive_options,
            previous=self.snapshot.previous,
        )
        logger.debug("sent snapshot %s", self.snapshot)


@dataclass(frozen=True)
class DestroyFilesystemTask:
    """Destroy a filesystem on the remote."""

    filesystem: FileSystem
    action: ClassVar[Action] = Action.DESTROY

    def run(self, replication: Replication) -> None:
        """Destroy the filesystem."""
        logger.info("destroying filesystem %s", self.filesystem.name)
        filesystem_ops.destroy(self.filesystem, ssh_command=replication.ssh_command)


@dataclass(frozen=True)
class DestroySnapshotTask:
    """Destroy a snapshot on the remote."""

    filesystem: FileSystem
    snapshot: Snapshot
    action: ClassVar[Action] = Action.DESTROY

    def run(self, replication: Replication) -> None:
        """Destroy the snapshot."""
        logger.info("destroying snapshot %s", self.snapshot)
        snapshot_ops.destroy(self.snapshot, ssh_command=replication.ssh_command)


# Separate types let mypy reject reading a snapshot off a task that has none,
# and a member without run() is rejected where execute() calls it.
Task = CreateFilesystemTask | SendSnapshotTask | DestroyFilesystemTask | DestroySnapshotTask
