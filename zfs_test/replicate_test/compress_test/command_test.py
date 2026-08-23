"""Test compress command generation."""

import pytest

from zfs.replicate.compress.command import command
from zfs.replicate.compress.type import Compression


class TestCommand:
    """Every compression maps to a command, so no member falls through."""

    @pytest.mark.parametrize("compression", list(Compression))
    def test_total(self, compression: Compression) -> None:
        """Ensure command is a total function."""
        command(compression)
