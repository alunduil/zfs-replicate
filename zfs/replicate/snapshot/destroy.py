"""ZFS Snapshot destruction."""

from typing import Optional

from .. import subprocess
from .type import Snapshot


def destroy(snapshot: Snapshot, ssh_command: Optional[str()]=None):
    """Destroy a snapshot."""

    command = _destroy(snapshot)

    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise RuntimeError(f"unable to destroy snapshot: {snapshot.name}: {error}")


def _destroy(snapshot: Snapshot) -> str():
    return f"/usr/bin/env zfs destroy '{snapshot.name}'"
