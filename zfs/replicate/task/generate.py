"""Replication Tasks."""

from typing import Dict, Iterable, List

from ..filesystem import FileSystem, local_filesystem, remote_filesystem
from ..list import venn
from ..snapshot import Snapshot
from .type import Action, Task


def _destroy_snapshots(destination: FileSystem, snapshots: Iterable[Snapshot]) -> List[Task]:
    return [Task(action=Action.DESTROY, filesystem=destination, snapshot=s) for s in snapshots]


def _send_snapshots(remote: FileSystem, snapshots: Iterable[Snapshot]) -> List[Task]:
    return [Task(action=Action.SEND, filesystem=remote, snapshot=s) for s in snapshots]


def generate(
    remote: FileSystem,
    local_snapshots: Dict[FileSystem, List[Snapshot]],
    remote_snapshots: Dict[FileSystem, List[Snapshot]],
    follow_delete: bool = False,
) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""
    tasks = []

    # zfs list reports remote filesystems prefixed with the remote's name.
    remote_snaps_by_local = {local_filesystem(remote, key): value for key, value in remote_snapshots.items()}

    for filesystem, local_snaps in local_snapshots.items():
        destination = remote_filesystem(remote, filesystem)

        if filesystem not in remote_snaps_by_local:
            tasks.append(Task(action=Action.CREATE, filesystem=destination, snapshot=None))
            tasks.extend(_send_snapshots(remote, local_snaps))
            continue

        lefts, middles, rights = venn(local_snaps, remote_snaps_by_local[filesystem])

        # execute() runs actions in the order they first appear, so these two
        # destroys cannot merge into `not middles or follow_delete`: one
        # belongs before the sends and the other after.
        if not middles:
            tasks.extend(_destroy_snapshots(destination, rights))

        tasks.extend(_send_snapshots(remote, lefts))

        if middles and follow_delete:
            tasks.extend(_destroy_snapshots(destination, rights))

    for filesystem, snapshots in remote_snaps_by_local.items():
        if filesystem not in local_snapshots:
            destination = remote_filesystem(remote, filesystem)

            tasks.extend(_destroy_snapshots(destination, snapshots))
            tasks.append(Task(action=Action.DESTROY, filesystem=destination, snapshot=None))

    return tasks
