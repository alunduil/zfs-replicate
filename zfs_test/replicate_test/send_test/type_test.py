"""Test send options flag rendering."""

import zfs.replicate.send.type as sut


class TestOptionsToFlags:
    """Each enabled setting contributes its flag, and nothing else does."""

    def test_raws_by_default(self) -> None:
        """Default options render only -w."""
        assert sut.Options().to_flags() == ["-w"]

    def test_omits_raw_when_disabled(self) -> None:
        """Disabling raw renders no flags."""
        assert not sut.Options(raw=False).to_flags()

    def test_adds_large_block(self) -> None:
        """Enabling large_block renders -L."""
        assert "-L" in sut.Options(large_block=True).to_flags()

    def test_adds_embed(self) -> None:
        """Enabling embed renders -e."""
        assert "-e" in sut.Options(embed=True).to_flags()

    def test_adds_compressed(self) -> None:
        """Enabling compressed renders -c."""
        assert "-c" in sut.Options(compressed=True).to_flags()

    def test_adds_props(self) -> None:
        """Enabling props renders -p."""
        assert "-p" in sut.Options(props=True).to_flags()
