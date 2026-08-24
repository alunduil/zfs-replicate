"""zfs_test.replicate_test.snapshot_test.strategies tests."""

from typing import Set, Tuple

from hypothesis import given

import zfs_test.replicate_test.snapshot_test.strategies as sut
from zfs.replicate.snapshot.type import Snapshot


class TestSnapshots:
    """Drawn snapshots vary their filesystem name."""

    def test_vary_filesystem(self) -> None:
        """SNAPSHOTS draws more than one filesystem name."""
        assert len(_drawn_filesystem_names()) > 1


class TestRebasedSnapshots:
    """Drawn pairs stand in for a local snapshot and its remote counterpart."""

    @given(sut.REBASED_SNAPSHOTS)
    def test_rebased(self, rebased: Tuple[Snapshot, Snapshot]) -> None:
        """REBASED_SNAPSHOTS draws pairs with differing filesystems that compare equal."""
        local, remote = rebased
        assert local.filesystem != remote.filesystem
        assert local == remote


def _drawn_filesystem_names() -> Set[str]:
    names: Set[str] = set()

    @given(sut.SNAPSHOTS)
    def collect(snapshot: Snapshot) -> None:
        names.add(snapshot.filesystem.name)

    collect()

    return names
