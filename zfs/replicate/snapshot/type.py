"""ZFS Snapshot Type."""

from typing import NamedTuple

from ..filesystem import FileSystem


class Snapshot(NamedTuple):
    """ZFS Snapshot Type."""

    filesystem: FileSystem
    name: str
    timestamp: int
