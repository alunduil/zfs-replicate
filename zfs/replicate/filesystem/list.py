"""ZFs FileSystem List."""

import re
from typing import List, Optional

from .. import subprocess
from ..command import Command, over_ssh, scrubbed
from ..error import ZFSReplicateError
from . import type  # pylint: disable=W0622
from .type import FileSystem

RE_WHITESPACE = re.compile(b"[ \t]+")


def list(  # pylint: disable=W0622
    filesystem: FileSystem, ssh_command: Optional[Command] = None
) -> List[FileSystem]:
    """List ZFS FileSystem."""
    command = _list(filesystem)
    if ssh_command is not None:
        command = over_ssh(ssh_command, command)

    result = subprocess.run(command)

    error = (
        result.stderr.strip(b"\n")
        .strip(b"\r")
        .replace(b"WARNING: ENABLED NONE CIPHER", b"")
    )

    if result.returncode:
        raise ZFSReplicateError(
            f"error encountered while listing filesystems of '{filesystem.name}': {error!r}",
            filesystem,
            error,
        )

    return _filesystems(result.stdout)


def _list(filesystem: FileSystem) -> Command:
    options = ["-H", "-o", "name,readonly", "-t", "filesystem,volume", "-r"]

    return scrubbed("zfs", "list", *options, filesystem.name)


def _filesystems(zfs_list_output: bytes) -> List[FileSystem]:
    return [_filesystem(x) for x in zfs_list_output.split(b"\n") if x != b""]


def _filesystem(zfs_list_line: bytes) -> FileSystem:
    name, readonly = RE_WHITESPACE.sub(b" ", zfs_list_line).split(b" ")

    return type.filesystem(name=name.decode("utf-8"), readonly=readonly == b"on")
