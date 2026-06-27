"""ZFS Receive Command Mapping."""

from ..filesystem import FileSystem
from .type import Options


def command(destination: FileSystem, options: Options) -> str:
    """Build the remote ``zfs receive`` invocation for a destination.

    Returns just the ``zfs receive`` call (like ``_send`` returns just
    ``zfs send``); ``snapshot.send`` wraps it with the SSH transport and the
    remote decompress prefix. Targets ``destination``, the already-resolved
    remote data set, so it carries no knowledge of how local data sets map
    onto the remote.
    """
    arguments = [*options.to_flags(), f"-d '{destination.name}'"]

    return f"/usr/bin/env - zfs receive {' '.join(arguments)}"
