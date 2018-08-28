"""zfs.replicate.snapshot tests"""

import datetime
import string
from typing import List

from hypothesis import given
from hypothesis.strategies import datetimes, lists, text, tuples

from zfs.replicate.snapshot.list import _snapshot, _snapshots
from zfs.replicate.snapshot.type import Snapshot

NOT_WHITESPACE = [x for x in string.printable if x not in string.whitespace and x != "@"]

snapshots = tuples(text(NOT_WHITESPACE), text(NOT_WHITESPACE), datetimes()).map(
    lambda x: Snapshot(x[0], x[1], x[2].replace(second=0, microsecond=0))
)  # pylint: disable=invalid-name


@given(lists(snapshots))
def test_snapshots(snapshots: List[Snapshot]) -> None:  # pylint: disable=redefined-outer-name
    """_snapshots"""

    output = "\n".join([_output(s) for s in snapshots])
    assert _snapshots(output) == snapshots


@given(snapshots)
def test_snapshot(snapshot: Snapshot) -> None:
    """_snapshot"""
    assert _snapshot(_output(snapshot)) == snapshot


def _output(snapshot: Snapshot) -> str:
    return f"{snapshot.filesystem}@{snapshot.name}\t{_format_datetime(snapshot.timestamp)}"


def _format_datetime(date_time: datetime.datetime) -> str:
    return f"{date_time.strftime('%a %b')} {date_time.day} {date_time.strftime('%H:%M %Y')}"
