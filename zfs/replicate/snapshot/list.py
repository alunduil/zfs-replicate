"""ZFS Snapshot listing."""

import functools
from typing import List, Optional

from .. import subprocess
from ..filesystem import FileSystem
from .type import Snapshot


@functools.lru_cache()
def list(  # pylint: disable=redefined-builtin
    filesystem: FileSystem, recursive: bool(), ssh_command: Optional[str()] = None
) -> List[Snapshot]:
    """List ZFS snapshots."""

    command = _list(filesystem, recursive)
    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    output, error = proc.communicate()
    error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

    if proc.returncode:
        raise RuntimeError(f"error encountered while listing snapshots of {filesystem}: {error}")

    return _snapshots(output)


def _list(filesystem: FileSystem, recursive: bool()) -> str():
    """ZFS List Snapshot command."""

    options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r"]

    if not recursive:
        options.append("-d 1")

    return f"/usr/bin/env zfs list {' '.join(options)} '{filesystem}'"


def _snapshots(zfs_list_output: str()) -> List[Snapshot]:
    return [_snapshot(x) for x in zfs_list_output.split("\n") if x != ""]


def _snapshot(zfs_list_line: str()) -> Optional[Snapshot]:
    name, timestamp = zfs_list_line.split("\t")
    filesystem, name = name.split("@")

    return Snapshot(filesystem=filesystem, name=name, timestamp=timestamp)
