"""ZFS Snapshot listing."""

from typing import List, Optional

from .. import subprocess
from ..filesystem import FileSystem
from .type import Snapshot


def list(  # pylint: disable=redefined-builtin
    filesystem: FileSystem, recursive: bool, ssh_command: Optional[str] = None
) -> List[Snapshot]:
    """List ZFS snapshots."""

    command = _list(filesystem, recursive)
    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    output, error = proc.communicate()
    if error is not None:
        error = error.strip(b"\n").strip(b"\r").replace(b"WARNING: ENABLED NONE CIPHER", b"")

    if proc.returncode:
        raise RuntimeError(f"error encountered while listing snapshots of {filesystem.name}: {error}")

    return _snapshots(output)


def _list(filesystem: FileSystem, recursive: bool) -> str:
    """ZFS List Snapshot command."""

    options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r"]

    if not recursive:
        options.append("-d 1")

    return f"/usr/bin/env - zfs list {' '.join(options)} '{filesystem.name}'"


def _snapshots(zfs_list_output: bytes) -> List[Snapshot]:
    return [_snapshot(x) for x in zfs_list_output.split(b"\n") if x != b""]


def _snapshot(zfs_list_line: bytes) -> Snapshot:
    name, timestamp = zfs_list_line.split(b"\t")
    filesystem, name = name.split(b"@")

    return Snapshot(
        filesystem=FileSystem(name=filesystem.decode("utf-8"), readonly=False),
        name=name.decode("utf-8"),
        timestamp=int(timestamp),
    )
