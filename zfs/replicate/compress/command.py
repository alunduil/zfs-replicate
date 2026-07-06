"""ZFS Replication Compression Command Mapping."""

from typing import Optional, Tuple

from ..command import Command, scrubbed
from .type import Compression


def command(compression: Compression) -> Tuple[Optional[Command], Optional[Command]]:
    """Compress and decompress commands for a compression.

    The first runs locally on the send side; the second runs on the remote
    receive side. ``OFF`` yields ``(None, None)`` -- no compression stage.
    """
    if compression == Compression.LZ4:
        return (scrubbed("lz4"), scrubbed("lz4", "-d"))

    if compression == Compression.OFF:
        return (None, None)

    raise ValueError(f"invalid compression: '{compression}'", compression)
