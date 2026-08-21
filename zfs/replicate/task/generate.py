"""Replication Tasks."""

from typing import Dict, Iterable, List

from ..filesystem import FileSystem, remote_filesystem
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
    local_snaps_by_destination = {
        remote_filesystem(remote, filesystem): snaps for filesystem, snaps in local_snapshots.items()
    }

    for destination, local_snaps in local_snaps_by_destination.items():
        if destination not in remote_snapshots:
            tasks.append(Task(action=Action.CREATE, filesystem=destination, snapshot=None))
            tasks.extend(_send_snapshots(remote, local_snaps))
            continue

        lefts, middles, rights = venn(local_snaps, remote_snapshots[destination])

        send_tasks = _send_snapshots(remote, lefts)
        destroy_tasks = _destroy_snapshots(destination, rights)

        # execute() runs actions in the order they first appear, so merging
        # these branches would change what runs first.
        if middles:
            tasks.extend(send_tasks)

            if follow_delete:
                tasks.extend(destroy_tasks)
        else:
            tasks.extend(destroy_tasks)
            tasks.extend(send_tasks)

    for destination, remote_snaps in remote_snapshots.items():
        if destination not in local_snaps_by_destination:
            tasks.extend(_destroy_snapshots(destination, remote_snaps))
            tasks.append(Task(action=Action.DESTROY, filesystem=destination, snapshot=None))

    return tasks
