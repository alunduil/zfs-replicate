"""Task Reporting Functions."""

import itertools

from typing import Dict, Sequence

from ..dataset import DataSet
from ..snapshot import Snapshot
from .type import Action, Task

LIMITS = {
    "dataset": 3,
    "action": 4,
    "snapshot": 12,
    }

AFTERS = {
    "dataset": "action",
    "action": "snapshot",
    }

def report(tasks: Sequence[Task]) -> str():
    """Pretty printed report on given Tasks."""

    datasets = itertools.groupby(tasks, key=lambda x: x.dataset)

    if len(datasets) > LIMITS["dataset"]:
        return _counts("dataset", tasks)

    return _report_dataset(datasets)


def _report_dataset(datasets: Dict[DataSet, Sequence[Task]]) -> str():
    output = ""

    for dataset, tasks in datasets:
        output += "dataset: {}\n".format(dataset)

        actions = itertools.groupby(tasks, key=lambda x: x.action)

        if len(actions) > LIMITS["action"]:
            output += _counts("action", tasks, indentation="    ")
        else:
            output += _report_action(actions, indentation=" - ")

    return output


def _report_action(actions: Dict[Action, Sequence[Task]], indentation: str()="") -> str():
    output = ""

    for action, tasks in actions:
        output += "{}action: {}\n".format(indentation, action)

        snapshots = itertools.groupby(tasks, key=lambda x: x.snapshot)

        if len(snapshots) > LIMITS["snapshot"]:
            output += _counts("snapshot", tasks, indentation=indentation + "   ")
        else:
            output += _report_snapshot(snapshots, indentation="   " + indentation)

    return output


def _report_snapshot(snapshots: Dict[Snapshot, Sequence[Task]], indentation: str()="") -> str():
    output = ""

    for snapshot in snapshots:
        output += "{}snapshot: {}\n".format(indentation, snapshot)

    return output


def _counts(current: str(), tasks: Sequence[Task], indentation: str()="") -> str():
    group = set([getattr(x, current) for x in tasks])

    output = "{}{}:{}\n".format(indentation, current, len(group))

    if current in AFTERS:
        output += _counts(AFTERS[current], tasks, indentation=indentation)

    return output
