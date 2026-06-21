"""zfs.replicate.task.generate tests."""

import itertools
import operator
from typing import List

from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot import Snapshot
from zfs.replicate.task.generate import generate
from zfs.replicate.task.type import Action
from zfs.replicate.task.type import Task
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS


def test_no_tasks() -> None:
    """generate(Any, {}, {}) == []."""
    assert not generate(filesystem("pool/filesystem"), {}, {})  # nosec


@given(lists(SNAPSHOTS))  # type: ignore[misc]
def test_empty_remotes(snapshots: List[Snapshot]) -> None:
    """Generate with empty remotes."""
    snapshots_by_fs = {
        k: list(v)
        for (k, v) in itertools.groupby(
            sorted(snapshots, key=operator.attrgetter("filesystem")),
            key=operator.attrgetter("filesystem"),
        )
    }

    result = generate(filesystem(""), snapshots_by_fs, {})

    assert len(  # nosec
        [t for t in result if t.action == Action.CREATE and t.snapshot is None]
    ) == len(snapshots_by_fs)
    assert len(  # nosec
        [t for t in result if t.action == Action.SEND and t.snapshot is not None]
    ) == sum(
        map(len, snapshots_by_fs.values()),
    )


@given(lists(SNAPSHOTS))  # type: ignore[misc]
def test_empty_locals(snapshots: List[Snapshot]) -> None:
    """Generate with empty locals."""
    snapshots_by_fs = {
        k: list(v)
        for (k, v) in itertools.groupby(
            sorted(snapshots, key=operator.attrgetter("filesystem")),
            key=operator.attrgetter("filesystem"),
        )
    }

    result = generate(filesystem(""), {}, snapshots_by_fs)

    assert len([t for t in result if t.action == Action.DESTROY]) == len(  # nosec
        snapshots_by_fs
    ) + sum(
        map(len, snapshots_by_fs.values()),
    )
    assert all(t.action == Action.DESTROY for t in result)  # nosec


def test_empty_locals_remote_prefixed_filesystem() -> None:
    """Destroy remote-only snapshots keyed by the remote filesystem name."""
    remote = filesystem("backup")
    remote_snapshot_filesystem = filesystem("backup/pool/filesystem")
    snapshot = Snapshot(
        filesystem=remote_snapshot_filesystem,
        name="snapshot",
        previous=None,
        timestamp=0,
    )

    result = generate(remote, {}, {remote_snapshot_filesystem: [snapshot]})

    assert result == [  # nosec
        Task(
            action=Action.DESTROY,
            filesystem=remote_snapshot_filesystem,
            snapshot=snapshot,
        ),
        Task(
            action=Action.DESTROY,
            filesystem=remote_snapshot_filesystem,
            snapshot=None,
        ),
    ]
