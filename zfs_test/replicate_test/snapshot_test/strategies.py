"""Snapshot Hypothesis Strategies."""

import string
from typing import Any, Dict

from hypothesis.strategies import (
    SearchStrategy,
    fixed_dictionaries,
    integers,
    none,
    text,
)

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot

# zfs list -H separates fields with \t and records with \n, and @ splits the filesystem from the snapshot name.
_ROUND_TRIP_SAFE = [x for x in string.printable if x not in string.whitespace and x != "@"]


def _non_empty_name(suffix: str) -> str:
    return f"a{suffix}"


_FILESYSTEMS = text(_ROUND_TRIP_SAFE).map(_non_empty_name).map(filesystem)

_SNAPSHOTS_DICT: Dict[str, SearchStrategy[Any]] = {
    "filesystem": _FILESYSTEMS,
    "name": text(_ROUND_TRIP_SAFE),
    "timestamp": integers(),
    "previous": none(),
}
SNAPSHOTS = fixed_dictionaries(_SNAPSHOTS_DICT).map(lambda kwargs: Snapshot(**kwargs))
