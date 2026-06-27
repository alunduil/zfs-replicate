"""Test receive command generation."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.receive.command import command
from zfs.replicate.receive.type import ReceiveOptions
from zfs.replicate.snapshot.type import Snapshot


def _snapshot() -> Snapshot:
    return Snapshot(
        filesystem=filesystem("pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )


def test_command_forces_by_default() -> None:
    """Default options embed -F and no optional flags."""
    result = command(filesystem("remote"), _snapshot(), "", ReceiveOptions())

    assert "zfs receive -F -d" in result  # nosec
    assert "-u" not in result  # nosec
    assert "-s" not in result  # nosec
    assert "-o " not in result  # nosec


def test_command_omits_force_when_disabled() -> None:
    """Disabling force omits -F."""
    result = command(filesystem("remote"), _snapshot(), "", ReceiveOptions(force=False))

    assert "-F" not in result  # nosec


def test_command_adds_no_mount() -> None:
    """Enabling no_mount embeds -u."""
    result = command(
        filesystem("remote"), _snapshot(), "", ReceiveOptions(no_mount=True)
    )

    assert "-u" in result  # nosec


def test_command_adds_resume() -> None:
    """Enabling resume embeds -s."""
    result = command(filesystem("remote"), _snapshot(), "", ReceiveOptions(resume=True))

    assert "-s" in result  # nosec


def test_command_sets_properties() -> None:
    """Each property becomes a -o KEY=VALUE argument."""
    result = command(
        filesystem("remote"),
        _snapshot(),
        "",
        ReceiveOptions(properties={"readonly": "on", "canmount": "noauto"}),
    )

    assert "-o readonly=on" in result  # nosec
    assert "-o canmount=noauto" in result  # nosec
