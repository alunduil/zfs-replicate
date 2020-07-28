"""ZFS FileSystem Operations."""

from .create import create as create  # pylint: disable=useless-import-alias
from .destroy import destroy as destroy  # pylint: disable=useless-import-alias
from .type import FileSystem as FileSystem  # pylint: disable=useless-import-alias
from .type import filesystem as filesystem  # pylint: disable=useless-import-alias


def remote_dataset(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote dataset for the remote and local pair."""

    return filesystem(name=remote.name + "/" + local.dataset)


def remote_filesystem(remote: FileSystem, local: FileSystem) -> FileSystem:
    """Remote filesystem for the remote and local pair."""

    return filesystem(name=remote.name + "/" + local.name)
