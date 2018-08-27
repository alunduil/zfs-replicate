"""Replication Tasks."""

import collections
from typing import Dict, List, overload

from ..filesystem import FileSystem
from ..list import venn
from ..snapshot import Snapshot

Task = collections.namedtuple("Task", [])


def generate(
    local_snapshots: Dict[FileSystem, List[Snapshot]],
    remote_snapshots: Dict[FileSystem, List[Snapshot]],
    follow_delete: bool = False,
) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""

    tasks = []

    for filesystem in local_snapshots:
        if filesystem not in remote_snapshots:
            tasks.append(create(filesystem))
            tasks.extend(map(send, local_snapshots[filesystem]))
            continue

        lefts, middles, rights = venn(local_snapshots[filesystem], remote_snapshots[filesystem])

        if not middles:
            tasks.extend(map(destroy, rights))

        tasks.extend(map(send, lefts))

        if middles and follow_delete:
            tasks.extend(map(destroy, rights))

    for dataset in remote_snapshots:
        if dataset not in local_snapshots:
            tasks.extend(map(destroy, remote_snapshots[dataset]))
            tasks.append(destroy(dataset))

    return tasks


def create(filesystem: FileSystem) -> Task:
    """Create a create Task."""

    return Task(action="create", dataset=filesystem, snapshot=None)


def send(snapshot: Snapshot) -> Task:
    """Create a send Task."""

    return Task(action="send", filesystem=snapshot.filesystem, snapshot=snapshot)


@overload
def destroy(snapshot: Snapshot) -> Task:
    """Create a destroy task."""

    return Task(action="destroy", filesystem=snapshot.filesystem, snapshot=snapshot)


@overload
def destroy(filesystem: FileSystem) -> Task:  # pylint: disable=function-redefined
    """Create a destroy task."""

    return Task(action="destroy", filesystem=filesystem, snapshot=None)
