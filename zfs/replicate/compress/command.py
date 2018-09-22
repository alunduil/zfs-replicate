"""ZFS Replication Compression Command Mapping."""

from typing import Tuple

from .type import Compression


def command(compression: Compression) -> Tuple[str, str]:
    """Compress and decompress command strings for compression."""

    if compression == Compression.LZ4:
        return (f"/usr/bin/env - lz4c | ", f"/usr/bin/env - lz4c -d | ")

    raise ValueError(f"invalid compression: '{compression}'", compression)
