"""ZFS Receive Command Mapping."""

from ..command import Command, scrubbed
from ..filesystem import FileSystem
from .type import Options


def command(destination: FileSystem, options: Options) -> Command:
    """Build the remote ``zfs receive`` invocation for a destination.

    Returns just the ``zfs receive`` call (like ``_send`` returns just
    ``zfs send``); ``snapshot.send`` wraps it with the SSH transport and the
    remote decompress prefix. Targets ``destination``, the already-resolved
    remote data set, so it carries no knowledge of how local data sets map
    onto the remote.
    """
    return scrubbed("zfs", "receive", *options.to_flags(), "-d", destination.name)
