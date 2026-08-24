"""ZFS Snapshot Type."""

from dataclasses import dataclass
from typing import Optional

from ..filesystem import FileSystem


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
        is_suffix = left == right or left.endswith("/" + right) or right.endswith("/" + left)

        return is_suffix and self.name == other.name and self.timestamp == other.timestamp

    def __hash__(self) -> int:
        """Hash of a Snapshot.

        Keys on the fields __eq__ compares exactly.  The filesystem is left out
        because equality accepts a suffix match, so equal Snapshots can carry
        different filesystem names; previous is left out because equality
        ignores it.
        """
        return hash((self.name, self.timestamp))
