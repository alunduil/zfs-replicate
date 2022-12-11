"""ZFS Replication Compression Command Mapping."""
from typing import Tuple

from .type import Compression


def command(compression: Compression) -> Tuple[str, str]:
    """Compress and decompress command strings for compression."""
    if compression == Compression.LZ4:
        return ("/usr/bin/env - lz4c | ", "/usr/bin/env - lz4c -d | ")

    if compression == Compression.OFF:
        return ("", "")

    raise ValueError(f"invalid compression: '{compression}'", compression)
