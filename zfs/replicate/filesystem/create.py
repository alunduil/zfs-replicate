"""ZFS FileSystem creation."""

import os.path

import click

from .. import subprocess
from ..list import inits
from .list import list  # pylint: disable=redefined-builtin
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: str) -> None:
    """Create a Remote FileSystem."""

    top_level = FileSystem(name=filesystem.name.split("/")[0], readonly=filesystem.readonly)

    filesystems = [x.name for x in list(top_level, ssh_command=ssh_command)]

    for head in inits(filesystem.name.split("/"))[1:]:
        path = os.path.join(*head)

        if path in filesystems:
            continue

        command = ssh_command + " " + _create(path)
        click.secho(command, fg="red")

        proc = subprocess.open(command)

        _, error = proc.communicate()
        error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

        if proc.returncode:
            raise RuntimeError(f"unable to create remote dataset: {filesystem}: {error}")


def _create(filesystem: str) -> str:
    return f"/usr/bin/env zfs create -o readonly=on {filesystem}"
