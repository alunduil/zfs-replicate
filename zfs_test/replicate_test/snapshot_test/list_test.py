"""zfs.replicate.snapshot tests."""
from typing import List

from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.snapshot.list import _snapshot, _snapshots
from zfs.replicate.snapshot.type import Snapshot
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


@given(lists(SNAPSHOTS))  # type: ignore[misc]
def test_snapshots(snapshots: List[Snapshot]) -> None:
    """_snapshots."""
    output = "\n".join([_output(s) for s in snapshots])
    assert _snapshots(output.encode()) == snapshots  # nosec


@given(lists(SNAPSHOTS, min_size=1))  # type: ignore[misc]
def test_snapshots_depth(snapshots: List[Snapshot]) -> None:
    """Ensure max depth of 2."""
    output = "\n".join([_output(s) for s in snapshots])
    assert max(map(_depth, _snapshots(output.encode()))) <= 2  # nosec


@given(SNAPSHOTS)  # type: ignore[misc]
def test_snapshot(snapshot: Snapshot) -> None:
    """_snapshot."""
    assert _snapshot(_output(snapshot).encode()) == snapshot  # nosec


def _output(snapshot: Snapshot) -> str:
    return f"{snapshot.filesystem.name}@{snapshot.name}\t{snapshot.timestamp}"


def _depth(snapshot: Snapshot) -> int:
    return 1 if snapshot.previous is None else 1 + _depth(snapshot.previous)
