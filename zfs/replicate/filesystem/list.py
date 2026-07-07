"""ZFs FileSystem List."""

import re
from typing import List

from .. import process
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from . import type  # pylint: disable=W0622
from .type import FileSystem

RE_WHITESPACE = re.compile(b"[ \t]+")


def list(  # pylint: disable=W0622
    filesystem: FileSystem, ssh_command: Command
) -> List[FileSystem]:
    """List ZFS FileSystem on the remote reachable through ``ssh_command``."""
    result = process.run(over_ssh(ssh_command, _list(filesystem)))

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

    return Command.with_empty_env("zfs", "list", *options, filesystem.name)


def _filesystems(zfs_list_output: bytes) -> List[FileSystem]:
    return [_filesystem(x) for x in zfs_list_output.split(b"\n") if x != b""]


def _filesystem(zfs_list_line: bytes) -> FileSystem:
    name, readonly = RE_WHITESPACE.sub(b" ", zfs_list_line).split(b" ")

    return type.filesystem(name=name.decode("utf-8"), readonly=readonly == b"on")
