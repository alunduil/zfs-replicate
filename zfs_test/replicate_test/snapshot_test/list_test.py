"""zfs.replicate.snapshot tests"""

from hypothesis import given
from hypothesis.strategies import datetimes, lists, text, tuples

from zfs.replicate.snapshot.list import _snapshot, _snapshots
from zfs.replicate.snapshot.type import Snapshot

snapshots = tuples(text(), text(), datetimes()).map(lambda x: Snapshot(*x))  # pylint: disable=invalid-name


# @given(lists(snapshots()))
# def test_snapshots(snapshots):  # pylint: disable=redefined-outer-name
#    """_snapshots"""
#
#    output = "\n".join([_output(s) for s in snapshots])
#    assert _snapshots(output) == snapshots
#
#
# @given(snapshots())
# def test_snapshot(snapshot):
#    """_snapshot"""
#    assert _snapshot(_output(snapshot)) == snapshot
#
#
# def _output(snapshot: Snapshot) -> str:
#    return f"{snapshot.dataset}@{snapshot.name}\t{snapshot.timestamp}"
