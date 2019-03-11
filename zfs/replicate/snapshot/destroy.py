"""ZFS Snapshot destruction."""

from .type import Snapshot
from .. import subprocess
from ..error import ZFSReplicateError


def destroy(snapshot: Snapshot, ssh_command: str) -> None:
    """Destroy a remote snapshot."""

    command = ssh_command + " " + _destroy(snapshot)

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise ZFSReplicateError(
            f"unable to destroy snapshot: '{snapshot.filesystem.name}@{snapshot.name}': {error}", snapshot, error
        )


def _destroy(snapshot: Snapshot) -> str:
    return f"/usr/bin/env - zfs destroy '{snapshot.filesystem.name}@{snapshot.name}'"
