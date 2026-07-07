"""ZFS FileSystem destruction."""

from .. import subprocess
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from .type import FileSystem


def destroy(filesystem: FileSystem, ssh_command: Command) -> None:
    """Destroy a remote filesystem."""
    result = subprocess.run(over_ssh(ssh_command, _destroy(filesystem)))

    if result.returncode:
        raise ZFSReplicateError(
            f"unable to destroy dataset: '{filesystem.dataset}': {result.stderr!r}",
            filesystem,
            result.stderr,
        )


def _destroy(filesystem: FileSystem) -> Command:
    return Command.with_empty_env("zfs", "destroy", "-r", filesystem.name)
