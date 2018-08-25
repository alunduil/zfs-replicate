"""ZFS Snapshot Operations."""

import collections
import functools

from typing import List, Optional

from . import subprocess
from .dataset import DataSet

Snapshot = collections.namedtuple("Snapshot", [])


# TODO exclusions to snapshot list.
@functools.lru_cache()
def list(dataset: DataSet, recursive: bool(), ssh_command: Optional[str()]=None) -> List[Snapshot]: # pylint: disable=redefined-builtin
    """List ZFS snapshots."""

    command = _list(dataset, recursive)
    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    output, error = proc.communicate()
    error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

    if proc.returncode:
        raise RuntimeError(
            "error encountered while listing snapshots of {}: {}".format(dataset, error)
            )

    return _snapshots(output)


def _list(dataset: DataSet, recursive: bool()) -> str():
    """ZFS List Snapshot command."""

    options = [
        "list",
        "-H",
        "-t snapshot",
        "-p",
        "-o name,creation",
        "-r",
        ]

    if not recursive:
        options.append("-d 1")

    return "/usr/bin/env zfs {} '{}'".format(" ".join(options), dataset)


def _snapshots(zfs_list_output: str()) -> List[Snapshot]:
    return [_snapshot(x) for x in zfs_list_output.split("\n") if x != ""]


def _snapshot(zfs_list_line: str()) -> Optional[Snapshot]:
    name, timestamp = zfs_list_line.split("\t")
    dataset, name = name.split("@")

    return Snapshot(name=name, dataset=dataset, timestamp=timestamp)
