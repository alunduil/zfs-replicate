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


def same_filesystem(left: FileSystem, right: FileSystem) -> bool:
    """Whether the two name the same filesystem, either side possibly rebased.

    zfs list reports a replica under the name remote_filesystem gave it, so the
    origin's name survives as a slash-aligned suffix.
    """
    return left.name == right.name or _is_rebase_of(left.name, right.name) or _is_rebase_of(right.name, left.name)


def _is_rebase_of(name: str, origin: str) -> bool:
    """Whether name is origin rebased under some other filesystem."""
    return name.endswith("/" + origin)
