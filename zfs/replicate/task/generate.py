"""Replication Tasks."""

import collections
from typing import Dict, List

from ..filesystem import FileSystem
from ..list import venn
from ..snapshot import Snapshot

Task = collections.namedtuple("Task", [])


def generate(
        local_snapshots: Dict[FileSystem, List[Snapshot]],
        remote_snapshots: Dict[FileSystem, List[Snapshot]],
        follow_delete: bool()=False
    ) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""

    tasks = []

    for local_snapshot in local_snapshots:
        if local_snapshot not in remote_snapshots:
            tasks.append(create(TODO))
            tasks.extend(map(send, local_snapshots[dataset]))
            continue

        lefts, middles, rights = venn(local_snapshots[dataset], remote_snapshots[dataset])

        # TODO Any better way to write this?
        if not middles:
            tasks.extend(map(destroy, rights))

        tasks.extend(map(send, lefts))

        if middles and follow_delete:
            tasks.extend(map(destroy, rights))
        # TODO ^^^

    for dataset in remote_snapshots:
        if dataset not in local_snapshots:
            tasks.extend(map(destroy, remote_snapshots[dataset]))
            tasks.append(destroy(dataset))

    return tasks


def create(filesystem: FileSystem) -> Task:
    """Create a create Task."""

    return Task(action="create", dataset=filesystem, snapshot=None)


def remove(filesystem: FileSystem) -> Task:
    """Create a remove Task."""

    return Task(action="remove", filesystem=filesystem, snapshot=None)


def send(snapshot: Snapshot) -> Task:
    """Create a send Task."""

    return Task(action="send", filesystem=snapshot.filesystem, snapshot=snapshot)


def destroy(snapshot: Snapshot) -> Task:
    """Create a destroy task."""

    return Task(action="destroy", filesystem=snapshot.filesystem, snapshot=snapshot)
