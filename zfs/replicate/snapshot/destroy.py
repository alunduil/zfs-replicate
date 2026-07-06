"""ZFS Snapshot destruction."""

from .. import subprocess
from ..command import Command, remote, scrubbed
from ..error import ZFSReplicateError
from .type import Snapshot


def destroy(snapshot: Snapshot, ssh_command: Command) -> None:
    """Destroy a remote snapshot."""
    result = subprocess.run(remote(ssh_command, _destroy(snapshot)))

    if result.returncode:
        raise ZFSReplicateError(
            f"unable to destroy snapshot: '{snapshot.filesystem.name}@{snapshot.name}': {result.stderr!r}",
            snapshot,
            result.stderr,
        )


def _destroy(snapshot: Snapshot) -> Command:
    return scrubbed("zfs", "destroy", f"{snapshot.filesystem.name}@{snapshot.name}")
