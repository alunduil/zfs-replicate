"""zfs_test.replicate_test.snapshot_test.strategies tests."""

from typing import Set

from hypothesis import given

import zfs_test.replicate_test.snapshot_test.strategies as sut
from zfs.replicate.snapshot.type import Snapshot


def test_snapshots_vary_filesystem() -> None:
    """SNAPSHOTS draws more than one filesystem name."""
    names: Set[str] = set()

    @given(sut.SNAPSHOTS)
    def collect(snapshot: Snapshot) -> None:
        names.add(snapshot.filesystem.name)

    collect()

    assert len(names) > 1
