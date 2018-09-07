"""ZFS FileSystem Operations."""

from .create import create
from .destroy import destroy
from .type import FileSystem, filesystem


def remote_dataset(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote dataset for the remote and local pair."""

    return type.filesystem(name=remote.name + "/" + local.dataset)


def remote_filesystem(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote filesystem for the remote and local pair."""

    return type.filesystem(name=remote.name + "/" + local.name)
