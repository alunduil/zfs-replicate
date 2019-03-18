"""ZFS Snapshot listing."""

from typing import List, Optional

from .. import subprocess
from ..error import ZFSReplicateError
from ..filesystem import FileSystem, filesystem
from .type import Snapshot


def list(  # pylint: disable=redefined-builtin
    filesystem: FileSystem, recursive: bool, ssh_command: Optional[str] = None  # pylint: disable=redefined-outer-name
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
        raise ZFSReplicateError(
            f"error encountered while listing snapshots of '{filesystem.name}': {error}", filesystem, error
        )

    return _snapshots(output)


def _list(filesystem: FileSystem, recursive: bool) -> str:  # pylint: disable=redefined-outer-name
    """ZFS List Snapshot command."""

    options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r"]

    if not recursive:
        options.append("-d 1")

    return f"/usr/bin/env - zfs list {' '.join(options)} '{filesystem.name}'"


def _snapshots(zfs_list_output: bytes) -> List[Snapshot]:
    snapshots = [_snapshot(x) for x in zfs_list_output.split(b"\n") if x != b""]

    if not snapshots:
        return snapshots

    snapshots[0] = _add_previous(snapshots[0], None)

    return [snapshots[0]] + [_add_previous(s, p) for s, p in zip(snapshots[1:], snapshots)]


def _snapshot(zfs_list_line: bytes) -> Snapshot:
    name, timestamp = zfs_list_line.split(b"\t")
    my_filesystem, name = name.split(b"@")

    return Snapshot(
        filesystem=filesystem(name=my_filesystem.decode("utf-8")),
        previous=None,
        name=name.decode("utf-8"),
        timestamp=int(timestamp),
    )


def _add_previous(snapshot: Snapshot, previous: Optional[Snapshot] = None) -> Snapshot:
    if previous is not None and snapshot.filesystem != previous.filesystem:
        previous = None

    return Snapshot(filesystem=snapshot.filesystem, name=snapshot.name, previous=previous, timestamp=snapshot.timestamp)
