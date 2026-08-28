"""zfs.replicate.task.generate tests."""

import itertools
import operator
from collections.abc import Iterable

from hypothesis import given
from hypothesis.strategies import lists

from zfs.replicate.filesystem import remote_filesystem
from zfs.replicate.filesystem.type import FileSystem, filesystem
from zfs.replicate.snapshot import Snapshot
from zfs.replicate.task.generate import generate
from zfs.replicate.task.type import Action, Task
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS

_REMOTE = filesystem("backup")
_LOCAL = filesystem("pool/filesystem")
_DESTINATION = remote_filesystem(_REMOTE, _LOCAL)


def _snapshot(fs: FileSystem, name: str, timestamp: int) -> Snapshot:
    return Snapshot(filesystem=fs, name=name, previous=None, timestamp=timestamp)


# Pinning task order needs a controlled overlap between the local and remote
# sets, which generation does not give.
_SHARED = _snapshot(_LOCAL, "shared", 1)
_UNSENT = _snapshot(_LOCAL, "unsent", 2)
_STALE = _snapshot(_DESTINATION, "stale", 3)


# A fixture cannot do this grouping: the value it groups is drawn per example
# rather than resolved once.
def _by_filesystem(snapshots: Iterable[Snapshot]) -> dict[FileSystem, list[Snapshot]]:
    return {
        k: list(v)
        for (k, v) in itertools.groupby(
            sorted(snapshots, key=operator.attrgetter("filesystem")),
            key=operator.attrgetter("filesystem"),
        )
    }


class TestGenerate:
    """The two listings decide which tasks run, and in what order."""

    def test_no_tasks(self) -> None:
        """generate(Any, {}, {}) == []."""
        assert not generate(filesystem("pool/filesystem"), {}, {})

    @given(lists(SNAPSHOTS))
    def test_empty_remotes(self, snapshots: list[Snapshot]) -> None:
        """Generate with empty remotes."""
        snapshots_by_fs = _by_filesystem(snapshots)

        result = generate(filesystem(""), snapshots_by_fs, {})

        assert len([t for t in result if t.action == Action.CREATE and t.snapshot is None]) == len(snapshots_by_fs)
        assert len([t for t in result if t.action == Action.SEND and t.snapshot is not None]) == sum(
            map(len, snapshots_by_fs.values()),
        )

    @given(lists(SNAPSHOTS))
    def test_empty_locals(self, snapshots: list[Snapshot]) -> None:
        """Generate with empty locals."""
        snapshots_by_fs = _by_filesystem(snapshots)

        result = generate(filesystem(""), {}, snapshots_by_fs)

        assert len([t for t in result if t.action == Action.DESTROY]) == len(snapshots_by_fs) + sum(
            map(len, snapshots_by_fs.values()),
        )
        assert all(t.action == Action.DESTROY for t in result)

    @given(lists(SNAPSHOTS))
    def test_empty_locals_remote_prefixed(self, snapshots: list[Snapshot]) -> None:
        """Generate with empty locals and prefixed remotes."""
        remote = filesystem("backup")
        snapshots_by_fs = {remote_filesystem(remote, k): v for (k, v) in _by_filesystem(snapshots).items()}

        result = generate(remote, {}, snapshots_by_fs)

        assert len([t for t in result if t.action == Action.DESTROY]) == len(snapshots_by_fs) + sum(
            map(len, snapshots_by_fs.values()),
        )
        assert all(t.filesystem in snapshots_by_fs for t in result)

    def test_empty_locals_remote_prefixed_filesystem(self) -> None:
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

        assert result == [
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

    def test_diverged_destroys_before_sending(self) -> None:
        """Without a snapshot in common, the destroys precede the sends."""
        result = generate(_REMOTE, {_LOCAL: [_UNSENT]}, {_DESTINATION: [_STALE]})

        assert [(t.action, t.snapshot) for t in result] == [
            (Action.DESTROY, _STALE),
            (Action.SEND, _UNSENT),
        ]

    def test_follow_delete_destroys_after_sending(self) -> None:
        """With a snapshot in common, follow_delete prunes after the sends."""
        result = generate(
            _REMOTE,
            {_LOCAL: [_SHARED, _UNSENT]},
            {_DESTINATION: [_SHARED, _STALE]},
            follow_delete=True,
        )

        assert [(t.action, t.snapshot) for t in result] == [
            (Action.SEND, _UNSENT),
            (Action.DESTROY, _STALE),
        ]

    def test_common_snapshot_keeps_the_remote_without_follow_delete(self) -> None:
        """Without follow_delete, a snapshot missing locally survives on the remote."""
        result = generate(_REMOTE, {_LOCAL: [_SHARED, _UNSENT]}, {_DESTINATION: [_SHARED, _STALE]})

        assert [(t.action, t.snapshot) for t in result] == [(Action.SEND, _UNSENT)]
