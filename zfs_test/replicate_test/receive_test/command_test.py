"""Test receive command generation."""

import zfs.replicate.receive.command as sut
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.type import Options


def test_command_forces_by_default() -> None:
    """Default options embed -F and no optional flags."""
    result = sut.command(filesystem("remote"), filesystem("pool/data"), "", Options())

    assert "zfs receive -F -d" in result  # nosec
    assert "-u" not in result  # nosec
    assert "-s" not in result  # nosec
    assert "-o " not in result  # nosec


def test_command_omits_force_when_disabled() -> None:
    """Disabling force omits -F."""
    result = sut.command(
        filesystem("remote"), filesystem("pool/data"), "", Options(force=False)
    )

    assert "-F" not in result  # nosec


def test_command_adds_no_mount() -> None:
    """Enabling no_mount embeds -u."""
    result = sut.command(
        filesystem("remote"), filesystem("pool/data"), "", Options(no_mount=True)
    )

    assert "-u" in result  # nosec


def test_command_adds_resume() -> None:
    """Enabling resume embeds -s."""
    result = sut.command(
        filesystem("remote"), filesystem("pool/data"), "", Options(resume=True)
    )

    assert "-s" in result  # nosec


def test_command_sets_properties() -> None:
    """Each property becomes a -o KEY=VALUE argument."""
    result = sut.command(
        filesystem("remote"),
        filesystem("pool/data"),
        "",
        Options(properties={"readonly": "on", "canmount": "noauto"}),
    )

    assert "-o readonly=on" in result  # nosec
    assert "-o canmount=noauto" in result  # nosec
