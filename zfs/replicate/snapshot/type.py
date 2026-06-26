"""ZFS Snapshot Type."""

from dataclasses import dataclass, field
from typing import Mapping, Optional

from ..filesystem import FileSystem


@dataclass(frozen=True)
class ReceiveOptions:
    """Options controlling the remote ``zfs receive``.

    ``force`` maps to ``-F``, ``no_mount`` to ``-u``, ``resume`` to ``-s``,
    and each ``properties`` entry to a ``-o KEY=VALUE`` argument.
    """

    force: bool = True
    no_mount: bool = False
    resume: bool = False
    properties: Mapping[str, str] = field(default_factory=dict)


@dataclass(frozen=True)
class Snapshot:
    """ZFS Snapshot Type."""

    filesystem: FileSystem
    name: str
    previous: Optional["Snapshot"]
    timestamp: int

    def __eq__(self, other: object) -> bool:
        """Equality of Snapshots."""
        if other is None:
            return False

        if not isinstance(other, Snapshot):
            raise NotImplementedError

        left = self.filesystem.name
        right = other.filesystem.name
        is_suffix = (
            left == right or left.endswith("/" + right) or right.endswith("/" + left)
        )

        return (
            is_suffix and self.name == other.name and self.timestamp == other.timestamp
        )
