"""zfs.replicate.snapshot.type tests."""

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot


class TestSnapshotEq:
    """``Snapshot.__eq__`` matches a local snapshot to its remote counterpart."""

    def test_ignore_previous(self) -> None:
        """Ignore previous in Snapshot equality."""
        zero = Snapshot(filesystem=filesystem(""), name="", previous=None, timestamp=0)
        previous = Snapshot(filesystem=filesystem(""), name="", previous=zero, timestamp=0)
        assert zero == previous

    def test_rejects_unaligned_suffix(self) -> None:
        """Snapshots on unrelated datasets sharing a name suffix are not equal."""
        local = Snapshot(filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0)
        remote = Snapshot(filesystem=filesystem("bigpool/data"), name="snap", previous=None, timestamp=0)
        assert local != remote

    def test_accepts_slash_aligned_suffix(self) -> None:
        """A remote rebased under another dataset stays equal to its local origin."""
        local = Snapshot(filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0)
        remote = Snapshot(
            filesystem=filesystem("backup/pool/data"),
            name="snap",
            previous=None,
            timestamp=0,
        )
        assert local == remote
