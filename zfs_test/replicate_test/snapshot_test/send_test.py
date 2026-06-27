"""zfs.replicate.snapshot.send tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.send.type import Options
from zfs.replicate.snapshot.send import _send
from zfs.replicate.snapshot.type import Snapshot


def _snapshot() -> Snapshot:
    return Snapshot(
        filesystem=filesystem("pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )


def test_send_renders_enabled_flags() -> None:
    """_send embeds the -X flag for each enabled send option."""
    command = _send(_snapshot(), options=Options(large_block=True, props=True))

    assert "-L" in command  # nosec
    assert "-p" in command  # nosec


def test_send_omits_disabled_flags() -> None:
    """_send leaves out the -X flag for each disabled send option."""
    command = _send(_snapshot(), options=Options(raw=False))

    assert "-w" not in command  # nosec
    assert "-L" not in command  # nosec
