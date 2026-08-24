"""zfs.replicate.snapshot.type tests."""

from hypothesis import given

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs_test.replicate_test.snapshot_test.strategies import REBASED_SNAPSHOTS


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


class TestSnapshotHash:
    """``Snapshot.__hash__`` holds equal Snapshots to one hash."""

    @given(REBASED_SNAPSHOTS)
    def test_ignores_filesystem(self, rebased: tuple[Snapshot, Snapshot]) -> None:
        """A remote rebased under another dataset hashes as its local origin; see #502."""
        local, remote = rebased
        assert hash(local) == hash(remote)

    @given(REBASED_SNAPSHOTS)
    def test_set_membership(self, rebased: tuple[Snapshot, Snapshot]) -> None:
        """A set holding a local snapshot contains its rebased remote; see #502."""
        local, remote = rebased
        assert remote in {local}  # noqa: FURB171 -- set lookup is the point; equality skips __hash__

    def test_ignores_previous(self) -> None:
        """Two snapshots differing only in previous share a hash; see #502."""
        zero = Snapshot(filesystem=filesystem(""), name="", previous=None, timestamp=0)
        previous = Snapshot(filesystem=filesystem(""), name="", previous=zero, timestamp=0)
        assert hash(zero) == hash(previous)
