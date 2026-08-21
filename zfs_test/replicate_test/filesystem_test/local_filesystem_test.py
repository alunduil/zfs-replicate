"""zfs.replicate.filesystem.local_filesystem tests."""

from hypothesis import example, given
from hypothesis.strategies import text

import zfs.replicate.filesystem as sut
from zfs.replicate.filesystem.type import filesystem

_NAMES = text("ab/", min_size=1)


@given(_NAMES)
@example("backup")
@example("backup/backup")
def test_inverts_remote_filesystem(name: str) -> None:
    """local_filesystem undoes remote_filesystem.

    The examples repeat the remote's name, where stripping the prefix and
    stripping every occurrence differ.
    """
    remote = filesystem("backup")
    local = filesystem(name)

    assert sut.local_filesystem(remote, sut.remote_filesystem(remote, local)) == local
