"""zfs.replicate.task.report tests."""

from hypothesis import given
from hypothesis.strategies import builds, lists, one_of

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task import report
from zfs.replicate.task.type import (
    CreateFilesystem,
    DestroyFilesystem,
    DestroySnapshot,
    SendSnapshot,
    Task,
)

TASKS = one_of(
    builds(CreateFilesystem),
    builds(SendSnapshot),
    builds(DestroyFilesystem),
    builds(DestroySnapshot),
)


class TestReport:
    """A report shows every group it can and counts the rest."""

    def test_empty_tasks(self) -> None:
        """Ensure no actions is an empty report."""
        assert report([]) == ""

    @given(tasks=lists(TASKS, min_size=1))
    def test_nonempty_tasks(self, tasks: list[Task]) -> None:
        """Ensure nonempty report from nonempty actions."""
        result = report(tasks)
        assert result != ""

    def test_more_action_runs_than_the_limit_are_counted(self) -> None:
        """Past the action limit, the report counts actions instead of naming them."""
        fs = filesystem("pool/alternating")
        tasks: list[Task] = []
        for index in range(6):
            snapshot = Snapshot(filesystem=fs, name=f"s{index}", previous=None, timestamp=index)
            tasks += [
                SendSnapshot(filesystem=fs, snapshot=snapshot),
                DestroySnapshot(filesystem=fs, snapshot=snapshot),
            ]

        result = report(tasks)

        # The counts form writes "action:2"; the detail form writes "action: ".
        assert "action:2" in result
        assert "action: " not in result

    def test_more_snapshots_than_the_limit_are_counted(self) -> None:
        """Past the snapshot limit, the report counts snapshots instead of naming them."""
        fs = filesystem("pool/wide")
        tasks: list[Task] = [
            SendSnapshot(
                filesystem=fs,
                snapshot=Snapshot(filesystem=fs, name=f"s{index}", previous=None, timestamp=index),
            )
            for index in range(15)
        ]

        result = report(tasks)

        assert "snapshot:15" in result
        assert "snapshot: " not in result
