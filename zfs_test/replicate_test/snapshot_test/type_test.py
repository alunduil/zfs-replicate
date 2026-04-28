"""zfs.replicate.snapshot.type tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot

from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


def test_eq_ignore_previous() -> None:
    """Ignore previous in Snapshot equality."""
    zero = Snapshot(filesystem=filesystem(""), name="", previous=None, timestamp=0)
    previous = Snapshot(filesystem=filesystem(""), name="", previous=zero, timestamp=0)
    assert zero == previous  # nosec


def test_filesystem_names_are_varied() -> None:
    """Sanity check: the filesystem strategy produces at least two distinct names."""
    seen = set()
    for _ in range(50):
        seen.add(str(SNAPSHOTS.example().filesystem))
    assert len(seen) >= 2, "Expected varied filesystem names, got: " + repr(seen)
