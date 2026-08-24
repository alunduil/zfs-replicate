"""Test compress command generation."""

from typing import cast

import pytest

from zfs.replicate.compress.command import command
from zfs.replicate.compress.type import Compression


class TestCommand:
    """Every compression maps to a command, so no member falls through."""

    @pytest.mark.parametrize("compression", list(Compression))
    def test_total(self, compression: Compression) -> None:
        """Ensure command is a total function."""
        command(compression)

    def test_rejects_a_compression_it_cannot_map(self) -> None:
        """An unmapped compression raises rather than silently dropping the stage."""
        # Every Compression member maps today, so reaching the guard that keeps a
        # later member from falling through means casting past the type.
        with pytest.raises(ValueError, match="invalid compression"):
            command(cast("Compression", "zstd"))
