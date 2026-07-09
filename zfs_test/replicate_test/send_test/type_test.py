"""Test send options flag rendering."""

import zfs.replicate.send.type as sut


def test_to_flags_raws_by_default() -> None:
    """Default options render only -w."""
    assert sut.Options().to_flags() == ["-w"]


def test_to_flags_omits_raw_when_disabled() -> None:
    """Disabling raw renders no flags."""
    assert not sut.Options(raw=False).to_flags()


def test_to_flags_adds_large_block() -> None:
    """Enabling large_block renders -L."""
    assert "-L" in sut.Options(large_block=True).to_flags()


def test_to_flags_adds_embed() -> None:
    """Enabling embed renders -e."""
    assert "-e" in sut.Options(embed=True).to_flags()


def test_to_flags_adds_compressed() -> None:
    """Enabling compressed renders -c."""
    assert "-c" in sut.Options(compressed=True).to_flags()


def test_to_flags_adds_props() -> None:
    """Enabling props renders -p."""
    assert "-p" in sut.Options(props=True).to_flags()
