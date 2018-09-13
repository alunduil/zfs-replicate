"""zfs.replicate.snapshot tests"""

import string
from typing import Dict, List

from hypothesis import given
from hypothesis.searchstrategy import SearchStrategy
from hypothesis.strategies import fixed_dictionaries, integers, lists, none, text

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.list import _snapshot, _snapshots
from zfs.replicate.snapshot.type import Snapshot

NOT_WHITESPACE = [x for x in string.printable if x not in string.whitespace and x != "@"]

filesystems = text(NOT_WHITESPACE).map(filesystem)

snapshots_dict: Dict[str, SearchStrategy] = {
    "filesystem": filesystems,
    "name": text(NOT_WHITESPACE),
    "timestamp": integers(),
    "previous": none(),
}
snapshots = fixed_dictionaries(snapshots_dict).map(lambda kwargs: Snapshot(**kwargs))  # pylint: disable=invalid-name


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
    return f"{snapshot.filesystem.name}@{snapshot.name}\t{snapshot.timestamp}"
