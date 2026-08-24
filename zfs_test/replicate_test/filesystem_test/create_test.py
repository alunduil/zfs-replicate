"""zfs.replicate.filesystem.create tests."""

import subprocess
from collections.abc import Callable

import pytest
from pytest_mock import MockerFixture

from zfs.replicate import process
from zfs.replicate.command import Command
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.create import create
from zfs.replicate.filesystem.type import FileSystem, filesystem


class TestCreate:
    """A create refuses a dataset it cannot name and surfaces the remote's reason when it fails."""

    @pytest.fixture
    def fails_after_listing(self, mocker: MockerFixture) -> Callable[[bytes], None]:
        """List an empty remote, then fail the create that follows with the given stderr."""

        def _fails_after_listing(error: bytes) -> None:
            mocker.patch.object(
                process,
                "run",
                side_effect=[
                    subprocess.CompletedProcess([], 0, b"", b""),
                    subprocess.CompletedProcess([], 1, b"", error),
                ],
            )

        return _fails_after_listing

    def test_refuses_a_dataset_it_cannot_name(self, ssh_command: Command) -> None:
        """Creating a FileSystem carrying no name names the dataset in the refusal."""
        # FileSystem declares name as str, so the guard is only reachable by
        # constructing one past the type.
        nameless = FileSystem(dataset="pool", name=None, readonly=False)  # type: ignore[arg-type]

        with pytest.raises(ZFSReplicateError) as raised:
            create(nameless, ssh_command)

        assert "refusing to create dataset: 'pool'" in raised.value.message

    def test_reports_stderr_without_line_endings(
        self,
        fails_after_listing: Callable[[bytes], None],
        ssh_command: Command,
    ) -> None:
        """A failed create names the dataset it could not create and the remote's reason."""
        fails_after_listing(b"cannot create 'pool/data': permission denied\r\n")

        with pytest.raises(ZFSReplicateError) as raised:
            create(filesystem("pool/data"), ssh_command)

        assert "unable to create remote dataset: 'pool'" in raised.value.message
        assert "permission denied" in raised.value.message
        assert "\\r" not in raised.value.message
        assert "\\n" not in raised.value.message
