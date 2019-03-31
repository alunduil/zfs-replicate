"""ZFS Replication Compression Command Mapping."""

from typing import Tuple

from .type import Compression


def command(compression: Compression) -> Tuple[str, str]:
    """Compress and decompress command strings for compression."""

    if compression == Compression.LZ4:
        return (f"lz4c | ", f"lz4c -d | ")

    raise ValueError(f"invalid compression: '{compression}'", compression)
