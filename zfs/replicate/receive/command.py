"""ZFS Receive Command Mapping."""

from .. import filesystem
from ..filesystem import FileSystem
from ..snapshot.type import Snapshot
from .type import ReceiveOptions


def command(
    remote: FileSystem,
    current: Snapshot,
    decompress_command: str,
    options: ReceiveOptions,
) -> str:
    """Receive command string for a snapshot and receive options."""
    destination = filesystem.remote_dataset(remote, current.filesystem)

    flags = []

    if options.force:
        flags.append("-F")

    if options.no_mount:
        flags.append("-u")

    if options.resume:
        flags.append("-s")

    for key, value in options.properties.items():
        flags.append(f"-o {key}={value}")

    flags.append("-d")

    return (
        f"{decompress_command}/usr/bin/env - zfs receive"
        f" {' '.join(flags)} '{destination.name}'"
    )
