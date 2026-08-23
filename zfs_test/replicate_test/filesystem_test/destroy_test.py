"""zfs.replicate.filesystem.destroy tests."""

from typing import Callable

import pytest

from zfs.replicate.command import Command
from zfs.replicate.error import ZFSReplicateError
from zfs.replicate.filesystem.destroy import destroy
from zfs.replicate.filesystem.type import FileSystem, filesystem


class TestDestroy:
    """``destroy`` removes a remote filesystem, reporting why a failed removal failed."""

    @pytest.fixture
    def dataset(self) -> FileSystem:
        """Name the remote filesystem a destroy targets."""
        return filesystem("pool/data")

    def test_reports_stderr_without_line_endings(
        self,
        fails_with: Callable[[bytes], None],
        ssh_command: Command,
        dataset: FileSystem,
    ) -> None:
        """A failed destroy names the reason without the shell's trailing line ending."""
        fails_with(b"cannot destroy 'pool/data': dataset is busy\r\n")

        with pytest.raises(ZFSReplicateError) as raised:
            destroy(dataset, ssh_command)

        assert "dataset is busy" in raised.value.message
        assert "\\r" not in raised.value.message
        assert "\\n" not in raised.value.message

    def test_reports_stderr_without_the_none_cipher_warning(
        self,
        fails_with: Callable[[bytes], None],
        ssh_command: Command,
        dataset: FileSystem,
    ) -> None:
        """The ssh banner does not reach a failed destroy's message."""
        fails_with(b"WARNING: ENABLED NONE CIPHERcannot destroy 'pool/data': dataset is busy")

        with pytest.raises(ZFSReplicateError) as raised:
            destroy(dataset, ssh_command)

        assert "dataset is busy" in raised.value.message
        assert "NONE CIPHER" not in raised.value.message
