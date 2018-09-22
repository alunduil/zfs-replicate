"""ZFS FileSystem creation."""

import os.path

from . import type  # pylint: disable=redefined-builtin
from .. import subprocess
from ..error import ZFSReplicateError
from ..list import inits
from .list import list  # pylint: disable=redefined-builtin
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: str) -> None:
    """Create a Remote FileSystem."""

    if filesystem.name is None:
        raise ZFSReplicateError(f"refusing to create dataset: '{filesystem.dataset}'", filesystem)

    top_level = type.filesystem(name=filesystem.dataset, readonly=filesystem.readonly)

    filesystems = [x.name for x in list(top_level, ssh_command=ssh_command)]

    for head in inits(filesystem.name.split("/"))[1:]:
        path = os.path.join(*head)

        if path in filesystems:
            continue

        command = ssh_command + " " + _create(path)

        proc = subprocess.open(command)

        _, error = proc.communicate()
        error = error.strip(b"\n").strip(b"\r").replace(b"WARNING: ENABLED NONE CIPHER", b"")

        if proc.returncode:
            if b"successfully created, but not mounted" in error:
                return  # Ignore this error.

            raise ZFSReplicateError(
                f"unable to create remote dataset: '{filesystem.dataset}': {error}", filesystem, error
            )


def _create(filesystem: str) -> str:
    return f"/usr/bin/env - zfs create -o readonly=on {filesystem}"
