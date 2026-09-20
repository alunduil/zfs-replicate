"""Types for Tasks."""

import logging
from dataclasses import dataclass

from .. import filesystem, snapshot
from ..filesystem import FileSystem
from ..snapshot import Snapshot
from .context import RunContext

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class CreateFilesystem:
    """A filesystem to create on the remote."""

    filesystem: FileSystem

    def run(self, context: RunContext) -> None:
        """Create the filesystem."""
        logger.info("creating filesystem %s", self.filesystem.name)
        filesystem.create(self.filesystem, ssh_command=context.ssh_command)


@dataclass(frozen=True)
class SendSnapshot:
    """A snapshot to send to the remote."""

    filesystem: FileSystem
    snapshot: Snapshot

    def run(self, context: RunContext) -> None:
        """Send the snapshot, incremental from its predecessor when it has one."""
        logger.info("sending snapshot %s", self.snapshot)
        snapshot.send(
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
class DestroyFilesystem:
    """A filesystem to destroy on the remote."""

    filesystem: FileSystem

    def run(self, context: RunContext) -> None:
        """Destroy the filesystem."""
        logger.info("destroying filesystem %s", self.filesystem.name)
        filesystem.destroy(self.filesystem, ssh_command=context.ssh_command)


@dataclass(frozen=True)
class DestroySnapshot:
    """A snapshot to destroy on the remote."""

    filesystem: FileSystem
    snapshot: Snapshot

    def run(self, context: RunContext) -> None:
        """Destroy the snapshot."""
        logger.info("destroying snapshot %s", self.snapshot)
        snapshot.destroy(self.snapshot, ssh_command=context.ssh_command)


# The union is the contract: execute() calls run() on it, so a member without
# one is rejected there, and report._action stays total because the set closes.
Task = CreateFilesystem | SendSnapshot | DestroyFilesystem | DestroySnapshot
