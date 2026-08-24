"""zfs.replicate.filesystem.list tests."""

from collections.abc import Callable

import pytest

from zfs.replicate.command import Command
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.list import list_filesystems
from zfs.replicate.filesystem.type import filesystem


class TestListFilesystems:
    """A failed list raises with the remote's reason, and nothing else."""

    def test_reports_stderr_without_line_endings(
        self,
        fails_with: Callable[[bytes], None],
        ssh_command: Command,
    ) -> None:
        """A failed list names the filesystem it could not read and the remote's reason."""
        fails_with(b"cannot open 'pool/data': dataset does not exist\r\n")

        with pytest.raises(ZFSReplicateError) as raised:
            list_filesystems(filesystem("pool/data"), ssh_command)

        assert "pool/data" in raised.value.message
        assert "dataset does not exist" in raised.value.message
        assert "\\r" not in raised.value.message
        assert "\\n" not in raised.value.message
