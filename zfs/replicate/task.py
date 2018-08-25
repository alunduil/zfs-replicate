"""Replication Tasks."""

import collections

from typing import List

from .snapshot import Snapshot

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
            tasks.append(Task(Action.SEND, local_snapshots[dataset][0]))
        else:



