"""zfs_test.replicate_test.snapshot_test.strategies tests."""

from hypothesis import given

import zfs_test.replicate_test.snapshot_test.strategies as sut
from zfs.replicate.snapshot.type import Snapshot


class TestSnapshots:
    """Drawn snapshots vary their filesystem name."""

    def test_vary_filesystem(self) -> None:
        """SNAPSHOTS draws more than one filesystem name."""
        assert len(_drawn_filesystem_names()) > 1


def _drawn_filesystem_names() -> set[str]:
    names: set[str] = set()

    @given(sut.SNAPSHOTS)
    def collect(snapshot: Snapshot) -> None:
        names.add(snapshot.filesystem.name)

    collect()

    return names
