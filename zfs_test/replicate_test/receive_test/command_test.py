"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


def test_command_assembles_receive_invocation() -> None:
    """Wrap the flags and quoted destination in a zfs receive invocation."""
    result = sut.command(filesystem("remote/pool"), Options())

    assert result == "/usr/bin/env - zfs receive -F -d 'remote/pool'"  # nosec
