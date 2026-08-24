"""ZFS FileSystem creation."""

import os.path

from .. import process
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from ..list import inits
from ..stderr import clean
from . import type
from .list import list
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: Command) -> None:
    """Create a Remote FileSystem."""
    if filesystem.name is None:
        msg = f"refusing to create dataset: '{filesystem.dataset}'"
        raise ZFSReplicateError(msg, filesystem)

    top_level = type.filesystem(name=filesystem.dataset, readonly=filesystem.readonly)

    filesystems = [x.name for x in list(top_level, ssh_command=ssh_command)]

    for head in inits(filesystem.name.split("/"))[1:]:
        path = os.path.join(*head)

        if path in filesystems:
            continue

        result = process.run(over_ssh(ssh_command, _create(path)))

        if result.returncode:
            error = clean(result.stderr)

            if b"successfully created, but not mounted" in error:
                return  # Ignore this error.

            msg = f"unable to create remote dataset: '{filesystem.dataset}': {error!r}"
            raise ZFSReplicateError(msg, filesystem, error)


def _create(filesystem: str) -> Command:
    return Command.with_empty_env("zfs", "create", "-o", "readonly=on", filesystem)
