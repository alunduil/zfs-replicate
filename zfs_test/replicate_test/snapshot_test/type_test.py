"""zfs.replicate.snapshot.type tests."""
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot


def test_eq_ignore_previous() -> None:
    """Ignore previous in Snapshot equality."""
    zero = Snapshot(filesystem=filesystem(""), name="", previous=None, timestamp=0)
    previous = Snapshot(filesystem=filesystem(""), name="", previous=zero, timestamp=0)
    assert zero == previous  # nosec
