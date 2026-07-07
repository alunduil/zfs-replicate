"""ZFS Snapshot destruction."""

from .. import process
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from .type import Snapshot


def destroy(snapshot: Snapshot, ssh_command: Command) -> None:
    """Destroy a remote snapshot."""
    result = process.run(over_ssh(ssh_command, _destroy(snapshot)))

    if result.returncode:
        raise ZFSReplicateError(
            f"unable to destroy snapshot: '{snapshot.filesystem.name}@{snapshot.name}': {result.stderr!r}",
            snapshot,
            result.stderr,
        )


def _destroy(snapshot: Snapshot) -> Command:
    return Command.with_empty_env(
        "zfs", "destroy", f"{snapshot.filesystem.name}@{snapshot.name}"
    )
