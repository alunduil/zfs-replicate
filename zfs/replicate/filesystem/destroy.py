"""ZFS FileSsytem destruction."""

from .. import subprocess
from ..error import ZFSReplicateError
from .type import FileSystem


def destroy(filesystem: FileSystem, ssh_command: str) -> None:
    """Destroy a remote fileystem."""

    command = ssh_command + " " + _destroy(filesystem)

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise ZFSReplicateError(f"unable to destroy dataset: '{filesystem.dataset}': {error}", filesystem, error)


def _destroy(filesystem: FileSystem) -> str:
    return f"/usr/bin/env - zfs destroy -r '{filesystem}'"
