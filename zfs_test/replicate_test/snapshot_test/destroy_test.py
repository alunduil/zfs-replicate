"""zfs.replicate.snapshot.destroy tests."""

import subprocess

import pytest
from pytest_mock import MockerFixture

from zfs.replicate import process
from zfs.replicate.command import Command
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.destroy import destroy
from zfs.replicate.snapshot.type import Snapshot


def _fails_with(mocker: MockerFixture, error: bytes) -> None:
    mocker.patch.object(process, "run", return_value=subprocess.CompletedProcess([], 1, b"", error))


class TestDestroy:
    """``destroy`` removes a remote snapshot, reporting why a failed removal failed."""

    @pytest.fixture
    def snapshot(self) -> Snapshot:
        """Name the remote snapshot a destroy targets."""
        return Snapshot(filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0)

    def test_reports_stderr_without_line_endings(
        self,
        mocker: MockerFixture,
        ssh_command: Command,
        snapshot: Snapshot,
    ) -> None:
        """A failed destroy names the reason without the shell's trailing line ending."""
        _fails_with(mocker, b"could not find any snapshots to destroy\r\n")

        with pytest.raises(ZFSReplicateError) as raised:
            destroy(snapshot, ssh_command)

        assert "could not find any snapshots to destroy" in raised.value.message
        assert "\\r" not in raised.value.message
        assert "\\n" not in raised.value.message

    def test_reports_stderr_without_the_none_cipher_warning(
        self,
        mocker: MockerFixture,
        ssh_command: Command,
        snapshot: Snapshot,
    ) -> None:
        """The ssh banner does not reach a failed destroy's message."""
        _fails_with(mocker, b"WARNING: ENABLED NONE CIPHERcould not find any snapshots to destroy")

        with pytest.raises(ZFSReplicateError) as raised:
            destroy(snapshot, ssh_command)

        assert "could not find any snapshots to destroy" in raised.value.message
        assert "NONE CIPHER" not in raised.value.message
