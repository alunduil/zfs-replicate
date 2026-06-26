"""zfs.replicate.snapshot.send tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.send import _receive, _send
from zfs.replicate.snapshot.type import ReceiveOptions, Snapshot


def test_send_includes_raw_by_default() -> None:
    """_send embeds --raw when raw is True."""
    snapshot = Snapshot(
        filesystem=filesystem("pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )
    assert "--raw" in _send(snapshot, raw=True)  # nosec


def test_send_omits_raw_when_disabled() -> None:
    """_send omits --raw when raw is False."""
    snapshot = Snapshot(
        filesystem=filesystem("pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )
    assert "--raw" not in _send(snapshot, raw=False)  # nosec


def _snapshot() -> Snapshot:
    return Snapshot(
        filesystem=filesystem("pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )


def test_receive_forces_by_default() -> None:
    """_receive embeds -F and no optional flags for default options."""
    command = _receive(filesystem("remote"), _snapshot(), "", ReceiveOptions())  # nosec

    assert "zfs receive -F -d" in command  # nosec
    assert "-u" not in command  # nosec
    assert "-s" not in command  # nosec
    assert "-o " not in command  # nosec


def test_receive_omits_force_when_disabled() -> None:
    """_receive omits -F when force is False."""
    command = _receive(
        filesystem("remote"), _snapshot(), "", ReceiveOptions(force=False)
    )

    assert "-F" not in command  # nosec


def test_receive_adds_no_mount() -> None:
    """_receive embeds -u when no_mount is True."""
    command = _receive(
        filesystem("remote"), _snapshot(), "", ReceiveOptions(no_mount=True)
    )

    assert "-u" in command  # nosec


def test_receive_adds_resume() -> None:
    """_receive embeds -s when resume is True."""
    command = _receive(
        filesystem("remote"), _snapshot(), "", ReceiveOptions(resume=True)
    )

    assert "-s" in command  # nosec


def test_receive_sets_properties() -> None:
    """_receive embeds a -o KEY=VALUE for each property."""
    command = _receive(
        filesystem("remote"),
        _snapshot(),
        "",
        ReceiveOptions(properties={"readonly": "on", "canmount": "noauto"}),
    )

    assert "-o readonly=on" in command  # nosec
    assert "-o canmount=noauto" in command  # nosec
