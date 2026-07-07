"""ZFS Receive Type."""

from dataclasses import dataclass, field
from typing import List, Mapping


@dataclass(frozen=True)
class Options:
    """Receive-side settings, built by the CLI and read by ``receive.command``.

    The defaults reproduce zfs-replicate's long-standing ``zfs receive -F``
    behaviour, so a bare ``Options()`` means "nothing changes from before".
    """

    force: bool = True
    no_mount: bool = False
    resume: bool = False
    properties: Mapping[str, str] = field(default_factory=dict)

    def to_flags(self) -> List[str]:
        """Render these settings as ``zfs receive`` argv tokens (the caller adds ``-d``).

        Each ``-o`` property is two tokens (``"-o"`` then ``"key=value"``) so
        the value stays a single argument no matter what it contains.
        """
        flags = []

        if self.force:
            flags.append("-F")

        if self.no_mount:
            flags.append("-u")

        if self.resume:
            flags.append("-s")

        for key, value in self.properties.items():
            flags.extend(["-o", f"{key}={value}"])

        return flags
