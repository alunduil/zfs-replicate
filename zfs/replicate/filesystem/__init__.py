"""ZFS FileSystem Operations."""

from .create import create as create
from .destroy import destroy as destroy
from .type import FileSystem as FileSystem
from .type import filesystem as filesystem


def remote_dataset(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote dataset for the remote and local pair."""
    return filesystem(name=remote.name + "/" + local.dataset)


def remote_filesystem(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote filesystem for the remote and local pair."""
    return filesystem(name=remote.name + "/" + local.name)


def local_filesystem(remote: FileSystem, remote_fs: FileSystem) -> FileSystem:
    """Local filesystem for a filesystem on the remote.

    Inverse of remote_filesystem.  Strips only a leading occurrence, so a
    local name that repeats the remote's survives.

    >>> local_filesystem(filesystem("backup"), filesystem("backup/pool/backup"))
    FileSystem(dataset='pool', name='pool/backup', readonly=False)
    """
    return filesystem(name=remote_fs.name.removeprefix(remote.name + "/"), readonly=remote_fs.readonly)
