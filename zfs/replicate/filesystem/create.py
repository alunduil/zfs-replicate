"""ZFS FileSystem creation."""

from typing import Optional

from .. import snapshot, subprocess
from ..list import inits
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: Optional[str] = None):
    """Create a FileSystem.

    If `ssh_command` is not None, it will be prefixed on the command run.  It is
    intended for this to run SSH but that is not enforced.

    """

    r_snaps = snapshot.list(filesystem, recursive=True)

    command = _create(filesystem)
    if ssh_command is not None:
        command = ssh_command + " " + command

    for part in inits(filesystem.split("/"))[1:]:
        if part in r_snaps:
            continue

        proc = subprocess.open(command)

        _, error = proc.communicate()
        error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

        if proc.returncode:
            raise RuntimeError(f"unable to create remote dataset: {filesystem}: {error}")


def _create(filesystem: FileSystem) -> str:
    return f"/usr/bin/env zfs create -o readonly=on {filesystem}"
