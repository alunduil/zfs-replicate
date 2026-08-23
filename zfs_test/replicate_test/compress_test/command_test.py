"""Test compress command generation."""

import pytest

from zfs.replicate.compress.command import command
from zfs.replicate.compress.type import Compression


class TestCommand:
    """``command`` renders the compressor and decompressor a compression names."""

    def test_total(self) -> None:
        """Ensure command is a total function."""
        for compression in Compression:
            try:
                command(compression)
            except ValueError:
                pytest.fail(f"unhandled case for {compression}")
