"""ZFS Snapshot Type."""

from dataclasses import dataclass

from ..filesystem import FileSystem, same_filesystem


@dataclass(frozen=True)
class Snapshot:
    """ZFS Snapshot Type."""

    filesystem: FileSystem
    name: str
    previous: "Snapshot | None"
    timestamp: int

    def __str__(self) -> str:
        """Name zfs knows the snapshot by, ``filesystem@name``."""
        return f"{self.filesystem.name}@{self.name}"

    def __eq__(self, other: object) -> bool:
        """Equality of Snapshots."""
        if not isinstance(other, Snapshot):
            return NotImplemented

        return same_filesystem(self.filesystem, other.filesystem) and self._key() == other._key()

    def __hash__(self) -> int:
        """Hash of a Snapshot."""
        return hash(self._key())

    def _key(self) -> tuple[str, int]:
        """Fields two equal Snapshots agree on exactly.

        The filesystem is absent because equality accepts a suffix match, so
        equal Snapshots can carry different filesystem names.
        """
        return self.name, self.timestamp
