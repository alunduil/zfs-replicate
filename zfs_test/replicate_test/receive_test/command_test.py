"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


def test_command_assembles_receive_invocation() -> None:
    """Wrap the flags and quoted destination in a zfs receive invocation."""
    result = sut.command(filesystem("remote/pool"), Options())

    assert result == "/usr/bin/env - zfs receive -F -d 'remote/pool'"  # nosec


def test_command_without_flags_abuts_destination() -> None:
    """Keep -d next to the destination even when no option flags render."""
    result = sut.command(filesystem("remote/pool"), Options(force=False))

    assert result == "/usr/bin/env - zfs receive -d 'remote/pool'"  # nosec
