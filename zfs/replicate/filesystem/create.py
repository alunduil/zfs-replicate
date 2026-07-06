"""ZFS FileSystem creation."""

import os.path

from .. import subprocess
from ..command import Command, remote, scrubbed
from ..error import ZFSReplicateError
from ..list import inits
from . import type  # pylint: disable=W0622
from .list import list  # pylint: disable=W0622
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: Command) -> None:
    """Create a Remote FileSystem."""
    if filesystem.name is None:
        raise ZFSReplicateError(
            f"refusing to create dataset: '{filesystem.dataset}'", filesystem
        )

    top_level = type.filesystem(name=filesystem.dataset, readonly=filesystem.readonly)

    filesystems = [x.name for x in list(top_level, ssh_command=ssh_command)]

    for head in inits(filesystem.name.split("/"))[1:]:
        path = os.path.join(*head)

        if path in filesystems:
            continue

        result = subprocess.run(remote(ssh_command, _create(path)))

        error = (
            result.stderr.strip(b"\n")
            .strip(b"\r")
            .replace(b"WARNING: ENABLED NONE CIPHER", b"")
        )

        if result.returncode:
            if b"successfully created, but not mounted" in error:
                return  # Ignore this error.

            raise ZFSReplicateError(
                f"unable to create remote dataset: '{filesystem.dataset}': {error!r}",
                filesystem,
                error,
            )


def _create(filesystem: str) -> Command:
    return scrubbed("zfs", "create", "-o", "readonly=on", filesystem)
