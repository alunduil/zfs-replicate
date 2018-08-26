"""ZFS Snapshot Operations."""

import collections
import functools

from typing import List, Optional

from . import subprocess
from .dataset import DataSet

Snapshot = collections.namedtuple("Snapshot", [])


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


def destroy(snapshot: Snapshot, ssh_command: Optional[str()]=None):
    """Destroy a snapshot."""

    command = _destroy(snapshot.name)

    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    _, error = proc.communicate()
    if proc.returncode:
        raise RuntimeError(
            "unable to destroy snapshot: {}: {}".format(snapshot.name, error)
            )

def _destroy(snapshot: Snapshot) -> str():
    return "/usr/bin/env zfs destroy '{}'".format(snapshot.name)


def send(snapshot: Snapshot, ssh_command: str(), previous: Optional[Snapshot]=None, compression: Compression=Compression.LZ4, follow_delete: bool()=False):
    send_command = _send(previous=previous, follow_delete=follow_delete)

    read, write = os.pipe()
    proc_pid = os.fork()
    if proc_pid == 0:
        os.close(read)
        os.dup2(write, 1)
        os.close(write)
        os.execv("/usr/bin/env", send_command)
    else:
        os.close(write)

    compress, decompress = compress.commands(compression)

    receive_command = compress + " | " + ssh_command + " " + decompress + " " + _receive(snapshot)

    read_fd = os.fdopen(read, "rb", 0)

    proc = subprocess.open(
            receive_command,
            shell=True,
            stdin=read_fd,
            
