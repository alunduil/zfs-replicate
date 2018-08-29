"""ZFS FileySystem Type."""

from typing import NamedTuple


class FileSystem(NamedTuple):
    """FileSystem Type."""

    name: str
    readonly: bool
