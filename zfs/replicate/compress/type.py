"""ZFS Replication Stream Compression."""

from enum import Enum, auto


class Compression(Enum):
    """Stream Compression Choices."""

    LZ4 = auto()
