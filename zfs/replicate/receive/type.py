"""ZFS Receive Type."""

from dataclasses import dataclass, field
from typing import Mapping


@dataclass(frozen=True)
class Options:
    """Options controlling the remote ``zfs receive``.

    ``force`` maps to ``-F``, ``no_mount`` to ``-u``, ``resume`` to ``-s``,
    and each ``properties`` entry to a ``-o KEY=VALUE`` argument.
    """

    force: bool = True
    no_mount: bool = False
    resume: bool = False
    properties: Mapping[str, str] = field(default_factory=dict)
