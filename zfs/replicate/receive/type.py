"""ZFS Receive Type."""

from dataclasses import dataclass, field
from typing import List, Mapping


@dataclass(frozen=True)
class Options:
    """Receive-side settings, built by the CLI and read by ``receive.command``.

    The defaults reproduce zfs-replicate's long-standing ``zfs receive -F``
    behaviour, so a bare ``Options()`` means "nothing changes from before".
    ``properties`` values are interpolated into the remote shell command
    unquoted, so keep them shell-safe.
    """

    force: bool = True
    no_mount: bool = False
    resume: bool = False
    properties: Mapping[str, str] = field(default_factory=dict)

    def to_flags(self) -> List[str]:
        """Render these settings as ``zfs receive`` flags (the caller adds ``-d``)."""
        flags = []

        if self.force:
            flags.append("-F")

        if self.no_mount:
            flags.append("-u")

        if self.resume:
            flags.append("-s")

        flags.extend(f"-o {key}={value}" for key, value in self.properties.items())

        return flags
