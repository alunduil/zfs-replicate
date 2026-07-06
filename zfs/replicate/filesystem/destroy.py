"""ZFS FileSystem destruction."""

from .. import subprocess
from ..command import Command, remote, scrubbed
from ..error import ZFSReplicateError
from .type import FileSystem


def destroy(filesystem: FileSystem, ssh_command: Command) -> None:
    """Destroy a remote filesystem."""
    result = subprocess.run(remote(ssh_command, _destroy(filesystem)))

    if result.returncode:
        raise ZFSReplicateError(
            f"unable to destroy dataset: '{filesystem.dataset}': {result.stderr!r}",
            filesystem,
            result.stderr,
        )


def _destroy(filesystem: FileSystem) -> Command:
    return scrubbed("zfs", "destroy", "-r", filesystem.name)
