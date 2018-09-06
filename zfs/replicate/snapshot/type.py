"""ZFS Snapshot Type."""

from typing import Any, NamedTuple, Optional

from ..filesystem import FileSystem


class Snapshot(NamedTuple):
    """ZFS Snapshot Type."""

    filesystem: FileSystem
    name: str
    previous: Optional[Any]  # mypy doesn't support nested types yet
    timestamp: int

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Snapshot):
            raise NotImplementedError

        is_suffix = self.filesystem.name.endswith(other.filesystem.name) or other.filesystem.name.endswith(
            self.filesystem.name
        )

        return is_suffix and self.name == other.name and self.timestamp == other.timestamp
