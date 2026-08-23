"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


class TestCommand:
    """``command`` renders the remote ``zfs receive`` invocation."""

    def test_assembles_receive_invocation(self) -> None:
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

    def test_without_flags_abuts_destination(self) -> None:
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

    def test_keeps_hostile_destination_as_one_token(self) -> None:
        """A destination with shell metacharacters stays a single argv token."""
        result = sut.command(filesystem("remote/pool a$b"), Options())

        assert result.argv[-1] == "remote/pool a$b"
        assert "'remote/pool a$b'" in result.render()
