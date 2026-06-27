"""ZFS Receive Type."""

from dataclasses import dataclass, field
from typing import Mapping


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
