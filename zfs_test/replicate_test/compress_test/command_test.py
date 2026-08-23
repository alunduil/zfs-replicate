"""Test compress command generation."""

import pytest

from zfs.replicate.compress.command import command
from zfs.replicate.compress.type import Compression


class TestCommand:
    """``command`` renders the compressor and decompressor a compression names."""

    @pytest.mark.parametrize("compression", list(Compression))
    def test_total(self, compression: Compression) -> None:
        """Ensure command is a total function."""
        command(compression)
