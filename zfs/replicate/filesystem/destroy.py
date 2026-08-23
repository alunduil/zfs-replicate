"""ZFS FileSystem destruction."""

from .. import process
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from ..stderr import clean
from .type import FileSystem


def destroy(filesystem: FileSystem, ssh_command: Command) -> None:
    """Destroy a remote filesystem."""
    result = process.run(over_ssh(ssh_command, _destroy(filesystem)))

    if result.returncode:
        error = clean(result.stderr)

        raise ZFSReplicateError(
            f"unable to destroy dataset: '{filesystem.dataset}': {error!r}",
            filesystem,
            error,
        )


def _destroy(filesystem: FileSystem) -> Command:
    return Command.with_empty_env("zfs", "destroy", "-r", filesystem.name)
