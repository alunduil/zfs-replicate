"""Replication Tasks."""

import collections

from typing import Dict, List

from ..dataset import DataSet
from ..list import venn
from ..snapshot import Snapshot

Task = collections.namedtuple("Task", [])


def generate(
        local_snapshots: Dict[DataSet, List[Snapshot]],
        remote_snapshots: Dict[DataSet, List[Snapshot]],
        follow_delete: bool()=False
    ) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""

    tasks = []

    for dataset in local_snapshots:
        if dataset not in remote_snapshots:
            tasks.append(create(dataset))
            tasks.extend(map(send, local_snapshots[dataset]))
            continue

        lefts, commons, rights = venn(local_snapshots[dataset], remote_snapshots[dataset])

        # TODO Any better way to write this?
        if not commons:
            tasks.extend(map(destroy, rights))

        tasks.extend(map(send, lefts))

        if commons and follow_delete:
            tasks.extend(map(destroy, rights))
        # TODO ^^^

    for dataset in remote_snapshots:
        if dataset not in local_snapshots:
            tasks.extend(map(destroy, remote_snapshots[dataset]))
            tasks.append(destroy(dataset))

    return tasks


def create(dataset: DataSet) -> Task:
    """Create a create Task."""

    return Task(action="create", dataset=dataset, snapshot=None)


def remove(dataset: DataSet) -> Task:
    """Create a remove Task."""

    return Task(action="remove", dataset=dataset, snapshot=None)


def send(snapshot: Snapshot) -> Task:
    """Create a send Task."""

    return Task(action="send", dataset=snapshot.dataset, snapshot=snapshot)


def destroy(snapshot: Snapshot) -> Task:
    """Create a destroy task."""

    return Task(action="destroy", dataset=snapshot.dataset, snapshot=snapshot)
