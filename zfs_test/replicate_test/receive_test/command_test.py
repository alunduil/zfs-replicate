"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


def test_command_assembles_receive_invocation() -> None:
    """Wrap the flags and destination in a zfs receive argv."""
    result = sut.command(filesystem("remote/pool"), Options())

    assert result.argv == [
        "/usr/bin/env",
        "-",
        "zfs",
        "receive",
        "-F",
        "-d",
        "remote/pool",
    ]


def test_command_without_flags_abuts_destination() -> None:
    """Keep -d next to the destination even when no option flags render."""
    result = sut.command(filesystem("remote/pool"), Options(force=False))

    assert result.argv == [
        "/usr/bin/env",
        "-",
        "zfs",
        "receive",
        "-d",
        "remote/pool",
    ]


def test_command_keeps_hostile_destination_as_one_token() -> None:
    """A destination with shell metacharacters stays a single argv token."""
    result = sut.command(filesystem("remote/pool a$b"), Options())

    assert result.argv[-1] == "remote/pool a$b"
    assert "'remote/pool a$b'" in result.render()
