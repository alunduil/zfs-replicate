"""zfs.replicate.task.type tests."""

from pytest_mock import MockerFixture

import zfs.replicate.task.type as sut
from zfs.replicate import filesystem, receive, send, snapshot
from zfs.replicate.command import Command
from zfs.replicate.compress import Compression
from zfs.replicate.snapshot.type import Snapshot
from zfs.replicate.task.context import RunContext

_FILESYSTEM = filesystem.filesystem("tank/data")
_PREVIOUS = Snapshot(filesystem=_FILESYSTEM, name="snap0", previous=None, timestamp=0)

_CONTEXT = RunContext(
    remote=filesystem.filesystem("backup"),
    ssh_command=Command("ssh", ["backup.example.com"]),
    compression=Compression.LZ4,
    send_options=send.Options(large_block=False, raw=True, embed=False, compressed=False, props=False),
    receive_options=receive.Options(force=True, no_mount=False, resume=False, properties={}),
)


class TestCreateFilesystem:
    """The task reaches the remote through filesystem.create."""

    def test_run_creates_its_own_filesystem(self, mocker: MockerFixture) -> None:
        """run() passes the task's filesystem and the context's ssh command."""
        create = mocker.patch.object(filesystem, "create")

        sut.CreateFilesystem(filesystem=_FILESYSTEM).run(_CONTEXT)

        create.assert_called_once_with(_FILESYSTEM, ssh_command=_CONTEXT.ssh_command)


class TestDestroyFilesystem:
    """The task reaches the remote through filesystem.destroy."""

    def test_run_destroys_its_own_filesystem(self, mocker: MockerFixture) -> None:
        """run() passes the task's filesystem and the context's ssh command."""
        destroy = mocker.patch.object(filesystem, "destroy")

        sut.DestroyFilesystem(filesystem=_FILESYSTEM).run(_CONTEXT)

        destroy.assert_called_once_with(_FILESYSTEM, ssh_command=_CONTEXT.ssh_command)


class TestDestroySnapshot:
    """The task destroys the snapshot, leaving the filesystem alone."""

    def test_run_destroys_the_snapshot_not_the_filesystem(self, mocker: MockerFixture) -> None:
        """run() calls snapshot.destroy and never filesystem.destroy."""
        destroy_snapshot = mocker.patch.object(snapshot, "destroy")
        destroy_filesystem = mocker.patch.object(filesystem, "destroy")

        sut.DestroySnapshot(filesystem=_FILESYSTEM, snapshot=_PREVIOUS).run(_CONTEXT)

        destroy_snapshot.assert_called_once_with(_PREVIOUS, ssh_command=_CONTEXT.ssh_command)
        destroy_filesystem.assert_not_called()


class TestSendSnapshot:
    """The task sends incrementally from the snapshot's own predecessor."""

    def test_run_sends_from_the_snapshots_predecessor(self, mocker: MockerFixture) -> None:
        """run() takes previous from the snapshot, not from the context."""
        send_snapshot = mocker.patch.object(snapshot, "send")
        current = Snapshot(filesystem=_FILESYSTEM, name="snap1", previous=_PREVIOUS, timestamp=1)

        sut.SendSnapshot(filesystem=_FILESYSTEM, snapshot=current).run(_CONTEXT)

        send_snapshot.assert_called_once_with(
            _CONTEXT.remote,
            current,
            ssh_command=_CONTEXT.ssh_command,
            compression=_CONTEXT.compression,
            send_options=_CONTEXT.send_options,
            receive_options=_CONTEXT.receive_options,
            previous=_PREVIOUS,
        )
