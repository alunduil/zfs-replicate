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

_NOT_WHITESPACE = [
    x for x in string.printable if x not in string.whitespace and x != "@"
]

_FILESYSTEMS = text(_NOT_WHITESPACE).map(lambda x: "a{x}").map(filesystem)

_SNAPSHOTS_DICT: Dict[str, SearchStrategy[Any]] = {
    "filesystem": _FILESYSTEMS,
    "name": text(_NOT_WHITESPACE),
    "timestamp": integers(),
    "previous": none(),
}
SNAPSHOTS = fixed_dictionaries(_SNAPSHOTS_DICT).map(lambda kwargs: Snapshot(**kwargs))
