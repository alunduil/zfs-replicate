"""ZFs FileSystem List."""

import re
from typing import List

import click

from .. import subprocess
from .type import FileSystem

RE_WHITESPACE = re.compile(b"[ \t]+")


def list(filesystem: FileSystem, ssh_command: str) -> List[FileSystem]:  # pylint: disable=redefined-builtin
    """List ZFS FileSystem."""

    command = _list(filesystem)
    if ssh_command is not None:
        command = ssh_command + " " + command

    click.secho(command, fg="red")

    proc = subprocess.open(command)

    output, error = proc.communicate()
    if error is not None:
        error = error.strip(b"\n").strip(b"\r").replace(b"WARNING: ENABLED NONE CIPHER", b"")

    if proc.returncode:
        raise RuntimeError(f"error encountered while listing filsystems of {filesystem.name}: {error}")

    return _filesystems(output)


def _list(filesystem: FileSystem) -> str:
    options = ["-H", "-o name,readonly", "-t filesystem,volume", "-r"]

    return f"/usr/bin/env zfs list {' '.join(options)} '{filesystem.name}'"


def _filesystems(zfs_list_output: bytes) -> List[FileSystem]:
    return [_filesystem(x) for x in zfs_list_output.split(b"\n") if x != b""]


def _filesystem(zfs_list_line: bytes) -> FileSystem:
    name, readonly = RE_WHITESPACE.sub(b" ", zfs_list_line).split(b" ")

    return FileSystem(name=name.decode("utf-8"), readonly=readonly == b"on")