"""ZFS FileSystem Operations."""

from .create import create
from .destroy import destroy
from .type import FileSystem


def remote_name(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote dataset name for the remote and local pair."""

    _, separator, tail = local.partition("/")
    return remote + separator + tail
