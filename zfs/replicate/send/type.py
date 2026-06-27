"""ZFS Send Type."""

from dataclasses import dataclass
from typing import List


@dataclass(frozen=True)
class Options:
    """Send-side settings, built by the CLI and read by ``snapshot._send``.

    The defaults reproduce zfs-replicate's long-standing ``zfs send --raw``
    behaviour, so a bare ``Options()`` means "nothing changes from before".
    """

    large_block: bool = False
    raw: bool = True
    embed: bool = False
    compressed: bool = False
    props: bool = False

    def to_flags(self) -> List[str]:
        """Render these settings as ``zfs send`` flags (the caller adds ``-i``)."""
        flags = []

        if self.large_block:
            flags.append("-L")

        if self.raw:
            flags.append("-w")

        if self.embed:
            flags.append("-e")

        if self.compressed:
            flags.append("-c")

        if self.props:
            flags.append("-p")

        return flags
