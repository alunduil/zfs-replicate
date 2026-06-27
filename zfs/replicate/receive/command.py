"""ZFS Receive Command Mapping."""

from ..filesystem import FileSystem
from .type import Options


def command(
    destination: FileSystem,
    decompress_command: str,
    options: Options,
) -> str:
    """Build the remote ``zfs receive`` half of the replication pipe.

    Returns a shell fragment to embed in an SSH command -- it is not run
    here; ``snapshot.send`` pipes the local ``zfs send`` into it. Targets
    ``destination``, the already-resolved remote data set, so it carries no
    knowledge of how local data sets map onto the remote.
    """
    flags = [*options.to_flags(), "-d"]

    return (
        f"{decompress_command}/usr/bin/env - zfs receive"
        f" {' '.join(flags)} '{destination.name}'"
    )
