"""ZFS Snapshot destruction."""

import click

from .. import subprocess
from .type import Snapshot


def destroy(snapshot: Snapshot, ssh_command: str) -> None:
    """Destroy a remote snapshot."""

    command = ssh_command + " " + _destroy(snapshot)
    click.secho(command, fg="red")

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise RuntimeError(f"unable to destroy snapshot: {snapshot.name}: {error}")


def _destroy(snapshot: Snapshot) -> str:
    return f"/usr/bin/env zfs destroy '{snapshot.name}'"
