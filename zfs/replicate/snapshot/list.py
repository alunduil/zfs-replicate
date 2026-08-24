"""ZFS Snapshot listing."""

from .. import process
from ..command import Command, over_ssh
from ..error import ZFSReplicateError
from ..filesystem import FileSystem, filesystem
from ..stderr import clean
from .type import Snapshot


def list_snapshots(
    filesystem: FileSystem,
    recursive: bool,
    ssh_command: Command | None = None,
) -> list[Snapshot]:
    """List ZFS snapshots."""
    command = _list(filesystem, recursive)
    if ssh_command is not None:
        command = over_ssh(ssh_command, command)

    result = process.run(command)

    if result.returncode:
        error = clean(result.stderr)

        raise ZFSReplicateError(
            f"error encountered while listing snapshots of '{filesystem.name}': {error!r}",
            filesystem,
            error,
        )

    return _snapshots(result.stdout)


def _list(filesystem: FileSystem, recursive: bool) -> Command:
    """ZFS List Snapshot command."""
    options = ["-H", "-t", "snapshot", "-p", "-o", "name,creation", "-r"]

    if not recursive:
        options.extend(["-d", "1"])

    return Command.with_empty_env("zfs", "list", *options, filesystem.name)


def _snapshots(zfs_list_output: bytes) -> list[Snapshot]:
    snapshots = [_snapshot(x) for x in zfs_list_output.split(b"\n") if x != b""]

    if not snapshots:
        return snapshots

    snapshots[0] = _add_previous(snapshots[0], None)

    # strict=False is deliberate: the offset slice is one element shorter, and stopping there is
    # what pairs each snapshot with its predecessor.
    return [snapshots[0]] + [_add_previous(s, p) for s, p in zip(snapshots[1:], snapshots, strict=False)]


def _snapshot(zfs_list_line: bytes) -> Snapshot:
    name, timestamp = zfs_list_line.split(b"\t")
    my_filesystem, name = name.split(b"@")

    return Snapshot(
        filesystem=filesystem(name=my_filesystem.decode("utf-8")),
        previous=None,
        name=name.decode("utf-8"),
        timestamp=int(timestamp),
    )


def _add_previous(snapshot: Snapshot, previous: Snapshot | None = None) -> Snapshot:
    if previous is not None and snapshot.filesystem != previous.filesystem:
        previous = None

    return Snapshot(
        filesystem=snapshot.filesystem,
        name=snapshot.name,
        previous=previous,
        timestamp=snapshot.timestamp,
    )
