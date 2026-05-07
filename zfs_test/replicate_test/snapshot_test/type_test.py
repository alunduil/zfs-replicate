"""zfs.replicate.snapshot.type tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot


def test_eq_ignore_previous() -> None:
    """Ignore previous in Snapshot equality."""
    zero = Snapshot(filesystem=filesystem(""), name="", previous=None, timestamp=0)
    previous = Snapshot(filesystem=filesystem(""), name="", previous=zero, timestamp=0)
    assert zero == previous  # nosec


def test_eq_rejects_unaligned_suffix() -> None:
    """Snapshots on unrelated datasets sharing a name suffix are not equal."""
    local = Snapshot(
        filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0
    )
    remote = Snapshot(
        filesystem=filesystem("bigpool/data"), name="snap", previous=None, timestamp=0
    )
    assert local != remote  # nosec


def test_eq_accepts_slash_aligned_suffix() -> None:
    """A remote rebased under another dataset stays equal to its local origin."""
    local = Snapshot(
        filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0
    )
    remote = Snapshot(
        filesystem=filesystem("backup/pool/data"),
        name="snap",
        previous=None,
        timestamp=0,
    )
    assert local == remote  # nosec
