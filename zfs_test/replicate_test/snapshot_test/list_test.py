"""zfs.replicate.snapshot tests"""

import string
from typing import List

from hypothesis import given
from hypothesis.strategies import integers, lists, text, tuples

from zfs.replicate.snapshot.list import _snapshot, _snapshots
from zfs.replicate.snapshot.type import Snapshot

NOT_WHITESPACE = [x for x in string.printable if x not in string.whitespace and x != "@"]

snapshots = tuples(text(NOT_WHITESPACE), text(NOT_WHITESPACE), integers()).map(
    lambda args: Snapshot(*args)
)  # pylint: disable=invalid-name


@given(lists(snapshots))
def test_snapshots(snapshots: List[Snapshot]) -> None:  # pylint: disable=redefined-outer-name
    """_snapshots"""

    output = "\n".join([_output(s) for s in snapshots])
    assert _snapshots(output.encode()) == snapshots


@given(snapshots)
def test_snapshot(snapshot: Snapshot) -> None:
    """_snapshot"""
    assert _snapshot(_output(snapshot).encode()) == snapshot


def _output(snapshot: Snapshot) -> str:
    return f"{snapshot.filesystem}@{snapshot.name}\t{snapshot.timestamp}"
