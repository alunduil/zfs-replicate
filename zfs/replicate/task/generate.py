"""Replication Tasks."""

from typing import Dict, List

from ..filesystem import FileSystem
from ..list import venn
from ..snapshot import Snapshot
from .type import Action, Task


def generate(
    local_snapshots: Dict[FileSystem, List[Snapshot]],
    remote_snapshots: Dict[FileSystem, List[Snapshot]],
    follow_delete: bool = False,
) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""

    tasks = []

    for filesystem in local_snapshots:
        if filesystem not in remote_snapshots:
            tasks.append(Task(action=Action.CREATE, filesystem=filesystem, snapshot=None))
            tasks.extend(
                [Task(action=Action.SEND, filesystem=s.filesystem, snapshot=s) for s in local_snapshots[filesystem]]
            )
            continue

        lefts: List[Snapshot]
        middles: List[Snapshot]
        rights: List[Snapshot]

        lefts, middles, rights = venn(local_snapshots[filesystem], remote_snapshots[filesystem])

        if not middles:
            tasks.extend([Task(action=Action.DESTROY, filesystem=s.filesystem, snapshot=s) for s in rights])

        tasks.extend([Task(action=Action.SEND, filesystem=s.filesystem, snapshot=s) for s in lefts])

        if middles and follow_delete:
            tasks.extend([Task(action=Action.DESTROY, filesystem=s.filesystem, snapshot=s) for s in rights])

    for filesystem in remote_snapshots:
        if filesystem not in local_snapshots:
            tasks.extend(
                [Task(action=Action.DESTROY, filesystem=s.filesystem, snapshot=s) for s in remote_snapshots[filesystem]]
            )
            tasks.append(Task(action=Action.DESTROY, filesystem=filesystem, snapshot=None))

    return tasks
