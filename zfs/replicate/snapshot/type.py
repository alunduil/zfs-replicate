"""ZFS Snapshot Type."""

from dataclasses import dataclass
from typing import Optional, Tuple

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

        return _same_filesystem(self.filesystem, other.filesystem) and self._key() == other._key()

    def __hash__(self) -> int:
        """Hash of a Snapshot."""
        return hash(self._key())

    def _key(self) -> Tuple[str, int]:
        """Fields two equal Snapshots agree on exactly.

        The filesystem is absent because equality accepts a suffix match, so
        equal Snapshots can carry different filesystem names.
        """
        return self.name, self.timestamp


def _same_filesystem(left: FileSystem, right: FileSystem) -> bool:
    """Whether the two name the same filesystem.

    zfs list reports a remote filesystem under the destination's name, leaving
    the local name a slash-aligned suffix of the remote one.
    """
    return left.name == right.name or left.name.endswith("/" + right.name) or right.name.endswith("/" + left.name)
