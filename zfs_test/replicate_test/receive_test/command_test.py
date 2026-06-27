"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


def test_command_assembles_receive_invocation() -> None:
    """Wrap the flags and quoted destination after the decompress prefix."""
    result = sut.command(filesystem("remote/pool"), "lz4 -d | ", Options())

    assert result == "lz4 -d | /usr/bin/env - zfs receive -F -d 'remote/pool'"  # nosec
