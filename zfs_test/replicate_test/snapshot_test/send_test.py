"""zfs.replicate.snapshot.send tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.send import _send
from zfs.replicate.snapshot.type import Snapshot


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
