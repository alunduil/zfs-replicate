"""ZFS Snapshot listing."""

import datetime
import re
from typing import List, Optional

from .. import subprocess
from ..filesystem import FileSystem
from .type import Snapshot

DATE_RE = re.compile(
    r"\w{3} (?P<month>\w{3})  ?(?P<day>\d{1,2})  ?(?P<hour>\d{1,2}):(?P<minute>\d{2}) (?P<year>\d{1,4})"
)


def list(  # pylint: disable=redefined-builtin
    filesystem: FileSystem, recursive: bool, ssh_command: Optional[str] = None
) -> List[Snapshot]:
    """List ZFS snapshots."""

    command = _list(filesystem, recursive)
    if ssh_command is not None:
        command = ssh_command + " " + command

    proc = subprocess.open(command)

    output, error = proc.communicate()
    error = error.strip("\n").strip("\r").replace("WARNING: ENABLED NONE CIPHER", "")

    if proc.returncode:
        raise RuntimeError(f"error encountered while listing snapshots of {filesystem}: {error}")

    return _snapshots(output)


def _list(filesystem: FileSystem, recursive: bool) -> str:
    """ZFS List Snapshot command."""

    options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r"]

    if not recursive:
        options.append("-d 1")

    return f"/usr/bin/env zfs list {' '.join(options)} '{filesystem}'"


def _snapshots(zfs_list_output: str) -> List[Snapshot]:
    return [_snapshot(x) for x in zfs_list_output.split("\n") if x != ""]


def _snapshot(zfs_list_line: str) -> Snapshot:
    name, timestamp = zfs_list_line.split("\t")
    filesystem, name = name.split("@")

    match = DATE_RE.match(timestamp)
    if match is None:
        raise RuntimeError(f"invalid timestamp, {timestamp}, found while parsing '{zfs_list_line}'")

    return Snapshot(
        filesystem=filesystem,
        name=name,
        timestamp=datetime.datetime(
            int(match.group("year")),
            _month(match.group("month")),
            int(match.group("day")),
            int(match.group("hour")),
            int(match.group("minute")),
        ),
    )


def _month(short: str) -> int:
    return {
        "jan": 1,
        "feb": 2,
        "mar": 3,
        "apr": 4,
        "may": 5,
        "jun": 6,
        "jul": 7,
        "aug": 8,
        "sep": 9,
        "oct": 10,
        "nov": 11,
        "dec": 12,
    }[short.lower()]
