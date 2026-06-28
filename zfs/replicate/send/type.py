"""ZFS Send Type."""

from dataclasses import dataclass
from typing import List


@dataclass(frozen=True)
class Options:
    """Operator-tunable ``zfs send`` stream flags, one boolean per flag.

    The CLI's ``--send-*`` group builds this and ``snapshot._send`` reads it
    via :meth:`to_flags` to assemble the send command; construct one directly
    only in tests. Defaults reproduce zfs-replicate's long-standing
    ``zfs send -w`` behaviour, so a bare ``Options()`` changes nothing.

    Each field enables a single ``zfs send`` flag:

    - ``large_block`` (``-L``): send blocks larger than 128 KiB whole, rather
      than splitting them -- needs the ``large_blocks`` feature on the
      destination.
    - ``raw`` (``-w``): send encrypted data sets without decrypting them.
    - ``embed`` (``-e``): send embedded-data blocks as-is instead of reading
      them back from disk -- needs the ``embedded_data`` feature.
    - ``compressed`` (``-c``): send already-compressed blocks as stored,
      skipping a decompress/recompress round trip.
    - ``props`` (``-p``): carry data set properties (compression, recordsize,
      and so on) in the stream.
    """

    large_block: bool = False
    raw: bool = True
    embed: bool = False
    compressed: bool = False
    props: bool = False

    def to_flags(self) -> List[str]:
        """Return the enabled options as ``zfs send`` tokens, e.g. ``["-L", "-w"]``.

        Emits flags in a stable ``-L -w -e -c -p`` order and omits the
        disabled ones. The structural ``-i previous`` (incremental source) is
        deliberately absent: ``snapshot._send`` appends it, since only it
        knows the previous snapshot.
        """
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
