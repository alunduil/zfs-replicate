"""ZFS Dataset Types."""

import collections

from typing import Optional

from . import snapshot
from . import subprocess
from .list import inits

DataSet = collections.namedtuple('DataSet', [])


def remote_name(remote: DataSet, local: DataSet) -> DataSet:
    """Remote dataset name for the remote and local pair."""

    _, separator, tail = local.partition["/"]
    return remote + separator + tail


def create(name: DataSet, ssh_command: Optional[str()]=None):
    """Create a dataset.

    If `ssh_command` is not None, it will be prefixed on the command run.  It is
    intended for this to run SSH but that is not enforced.

    """

    remotes = snapshot.list(name, recursive=True)

    command = _create(name)
    if ssh_command is not None:
        command = ssh_command + " " + command

    for dataset in inits(name.split("/"))[1:]:
        if dataset in remotes:
            continue

        proc = subprocess.open(command)

        _, error = proc.communicate()
        error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

        if proc.returncode:
            raise RuntimeError(
                "unable to create remote dataset: {}: {}".format(name, error)
                )

def _create(name: DataSet) -> str():
    return "/usr/bin/env zfs create -o readonly=on {}".format(name)


def destroy(name: DataSet, ssh_command: Optional[str()]=None):
    """Destroy a dataset."""

    command = _destroy(name)

    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise RuntimeError(
            "unable to destroy dataset: {}: {}".format(name, error)
            )


def _destroy(name: DataSet) -> str():
    return "/usr/bin/env zfs destroy -r '{}'".format(name)
