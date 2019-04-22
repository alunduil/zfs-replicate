from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.task.generate import *  # pylint: disable=unused-wildcard-import,wildcard-import


def test_no_tasks() -> None:
    """generate(Any, {}, {}) == []"""

    assert [] == generate(filesystem("pool/filesystem"), {}, {})
