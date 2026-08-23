"""ZFS Replication Compression Command Mapping."""

from dataclasses import dataclass
from typing import Optional

from ..command import Command
from .type import Compression


@dataclass(frozen=True)
class Commands:
    """The pair of commands a compression stage needs.

    The two are not interchangeable: ``compress`` runs locally on the sending
    side, ``decompress`` runs on the far side of ssh. Naming the slots keeps a
    call site from transposing them, and making both fields required means a
    half-configured stage cannot be built. Compression is on or off, never
    one-sided.
    """

    compress: Command
    decompress: Command


def command(compression: Compression) -> Optional[Commands]:
    """Map a compression to its local compress and remote decompress commands.

    ``OFF`` yields ``None`` -- no compression stage.
    """
    if compression == Compression.LZ4:
        return Commands(
            compress=Command.with_empty_env("lz4"),
            decompress=Command.with_empty_env("lz4", "-d"),
        )

    if compression == Compression.OFF:
        return None

    raise ValueError(f"invalid compression: '{compression}'", compression)
