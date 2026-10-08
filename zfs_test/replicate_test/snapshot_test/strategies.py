"""Snapshot Hypothesis Strategies."""

import string
from dataclasses import replace

from hypothesis.strategies import builds, integers, none, text

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot

# zfs list -H separates fields with \t and records with \n, and @ splits the filesystem from the snapshot name.
_ROUND_TRIP_SAFE = [x for x in string.printable if x not in string.whitespace and x != "@"]


def _non_empty_name(suffix: str) -> str:
    return f"a{suffix}"


_NAMES = text(_ROUND_TRIP_SAFE).map(_non_empty_name)

SNAPSHOTS = builds(
    Snapshot,
    filesystem=_NAMES.map(filesystem),
    name=text(_ROUND_TRIP_SAFE),
    timestamp=integers(),
    previous=none(),
)


def _rebase(snapshot: Snapshot, parent: str) -> tuple[Snapshot, Snapshot]:
    return snapshot, replace(snapshot, filesystem=filesystem(f"{parent}/{snapshot.filesystem.name}"))


# Pairs whose fields differ but which Snapshot equality treats as one.
REBASED_SNAPSHOTS = builds(_rebase, SNAPSHOTS, _NAMES)
