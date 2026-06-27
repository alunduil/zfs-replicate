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
    """Build the remote ``zfs receive`` half of the replication pipe.

    Returns a shell fragment to embed in an SSH command -- it is not run
    here; ``snapshot.send`` pipes the local ``zfs send`` into it. Takes the
    source filesystem rather than the snapshot, since the receive side only
    needs the destination (derived via ``filesystem.remote_dataset``); that
    keeps ``receive`` independent of ``snapshot``.
    """
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
