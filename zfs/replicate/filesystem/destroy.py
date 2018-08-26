"""ZFS FileSsytem destruction."""

from typing import Optional

from .. import subprocess
from .type import FileSystem


def destroy(filesystem: FileSystem, ssh_command: Optional[str()] = None):
    """Destroy a fileystem.

    If `ssh_command` is not None, it will be prefixed on the command run.  It is
    intended for this to run SSH but that is not enforced.

    """

    command = _destroy(filesystem)

    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise RuntimeError(f"unable to destroy dataset: {filesystem}: {error}")


def _destroy(filesystem: FileSystem) -> str():
    return f"/usr/bin/env zfs destroy -r '{filesystem}'"
