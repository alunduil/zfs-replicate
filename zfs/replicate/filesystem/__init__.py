"""ZFS FileSystem Operations."""
from .create import create as create  # noqa: F401
from .destroy import destroy as destroy  # noqa: F401
from .type import FileSystem as FileSystem
from .type import filesystem as filesystem


def remote_dataset(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote dataset for the remote and local pair."""
    return filesystem(name=remote.name + "/" + local.dataset)


def remote_filesystem(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote filesystem for the remote and local pair."""
    return filesystem(name=remote.name + "/" + local.name)
