"""ZFS Receive Command Mapping."""

from .. import filesystem
from ..filesystem import FileSystem
from .type import Options


def command(
    remote: FileSystem,
    local: FileSystem,
    decompress_command: str,
    options: Options,
) -> str:
    """Receive command string for a destination and receive options."""
    destination = filesystem.remote_dataset(remote, local)

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
