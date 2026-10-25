"""ZFS Replication Compression Command Mapping."""

from dataclasses import dataclass
from typing import assert_never

from ..command import Command
from .type import Compression


@dataclass(frozen=True)
class Commands:
    """The two halves of a compression stage, one per side of the ssh boundary.

    ``compress`` runs locally on the sending side; ``decompress`` runs remotely,
    ahead of ``zfs receive``. Compression is on or off, never one-sided, so a
    stage always carries both.
    """

    compress: Command
    decompress: Command


def command(compression: Compression) -> Commands | None:
    """Map a compression to its local compress and remote decompress commands.

    ``OFF`` yields ``None`` -- no compression stage.
    """
    match compression:
        case Compression.LZ4:
            return Commands(
                compress=Command.with_empty_env("lz4"),
                decompress=Command.with_empty_env("lz4", "-d"),
            )
        case Compression.OFF:
            return None
        case _:
            assert_never(compression)
