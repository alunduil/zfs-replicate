"""ZFS FileSystem creation."""

import os.path

import click

from .. import snapshot, subprocess
from ..list import inits
from .type import FileSystem


def create(filesystem: FileSystem, ssh_command: str) -> None:
    """Create a Remote FileSystem."""

    r_snaps = snapshot.list(filesystem, recursive=True)

    for head in inits(filesystem.split("/"))[1:]:
        path = os.path.join(*head)

        if path in r_snaps:
            continue

        command = ssh_command + " " + _create(path)
        click.echo(command)

        proc = subprocess.open(command)

        _, error = proc.communicate()
        error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

        if proc.returncode:
            raise RuntimeError(f"unable to create remote dataset: {filesystem}: {error}")


def _create(filesystem: FileSystem) -> str:
    return f"/usr/bin/env zfs create -o readonly=on {filesystem}"
