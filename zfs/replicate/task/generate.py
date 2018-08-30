"""Replication Tasks."""

from typing import Dict, List, Optional

from ..filesystem import FileSystem, remote_name
from ..list import venn
from ..snapshot import Snapshot
from .type import Action, Task


def generate(
    remote: FileSystem,
    local_snapshots: Dict[FileSystem, List[Snapshot]],
    remote_snapshots: Dict[FileSystem, List[Snapshot]],
    follow_delete: bool = False,
) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""

    tasks = []

    for filesystem in local_snapshots:
        if filesystem not in remote_snapshots:
            tasks.append(_task(Action.CREATE, remote_name(remote, filesystem)))
            tasks.extend([_task(Action.SEND, remote, s) for s in local_snapshots[filesystem]])
            continue

        lefts: List[Snapshot]
        middles: List[Snapshot]
        rights: List[Snapshot]

        lefts, middles, rights = venn(local_snapshots[filesystem], remote_snapshots[filesystem])

        if not middles:
            tasks.extend([_task(Action.DESTROY, remote, s) for s in rights])

        tasks.extend([_task(Action.SEND, remote, s) for s in lefts])

        if middles and follow_delete:
            tasks.extend([_task(Action.DESTROY, remote, s) for s in rights])

    for filesystem in remote_snapshots:
        if filesystem not in local_snapshots:
            tasks.extend([_task(Action.DESTROY, remote, s) for s in remote_snapshots[filesystem]])
            tasks.append(_task(Action.DESTROY, remote_name(remote, filesystem)))

    return tasks


def _task(action: Action, destination: FileSystem, snapshot: Optional[Snapshot] = None) -> Task:
    if snapshot is not None:
        destination = remote_name(destination, snapshot.filesystem)

    return Task(action=action, filesystem=destination, snapshot=snapshot)
