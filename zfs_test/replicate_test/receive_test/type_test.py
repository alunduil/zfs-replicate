"""Test receive options flag rendering."""

import zfs.replicate.receive.type as sut


def test_to_flags_forces_by_default() -> None:
    """Default options render only -F."""
    assert sut.Options().to_flags() == ["-F"]  # nosec


def test_to_flags_omits_force_when_disabled() -> None:
    """Disabling force renders no flags."""
    assert not sut.Options(force=False).to_flags()  # nosec


def test_to_flags_adds_no_mount() -> None:
    """Enabling no_mount renders -u."""
    assert "-u" in sut.Options(no_mount=True).to_flags()  # nosec


def test_to_flags_adds_resume() -> None:
    """Enabling resume renders -s."""
    assert "-s" in sut.Options(resume=True).to_flags()  # nosec


def test_to_flags_renders_properties() -> None:
    """Each property renders a -o token followed by its KEY=VALUE token."""
    flags = sut.Options(
        force=False, properties={"readonly": "on", "canmount": "noauto"}
    ).to_flags()

    assert flags == ["-o", "readonly=on", "-o", "canmount=noauto"]  # nosec
