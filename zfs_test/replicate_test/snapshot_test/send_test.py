"""ZFS Snapshot Send Test."""

import unittest.mock as mock

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.type import Snapshot


class TestSend:
    """Test send functionality."""

    def test_receive_command_includes_resumable_flag(self):
        """Test that the receive command includes the -s flag for resumable receives."""
        # Import the actual module file to access private functions
        from zfs.replicate.snapshot.send import _receive

        # Create test objects
        remote = filesystem("test_remote")
        current = Snapshot(filesystem("test_local"), "test_snap", None, 0)

        # Call the _receive function
        result = _receive(remote, current, "")

        # Check that the -s flag is included
        assert "-s" in result
        assert "zfs receive -s -F -d" in result

    def test_is_resumable_failure_detects_resumable_errors(self):
        """Test that resumable errors are correctly detected."""
        from zfs.replicate.snapshot.send import _is_resumable_failure

        # Test cases for resumable errors
        resumable_errors = [
            b"cannot receive new filesystem stream",
            b"resumable receive",
            b"incomplete stream",
            b"connection reset",
            b"broken pipe",
        ]

        for error in resumable_errors:
            assert _is_resumable_failure(error)

        # Test case for non-resumable error
        non_resumable_error = b"permission denied"
        assert not _is_resumable_failure(non_resumable_error)

    def test_send_resume_command_format(self):
        """Test that the resume send command is correctly formatted."""
        from zfs.replicate.snapshot.send import _send_resume

        token = "test_token_123"
        result = _send_resume(token)

        expected = "/usr/bin/env - zfs send -w -t 'test_token_123'"
        assert result == expected

    @mock.patch("zfs.replicate.snapshot.send.subprocess.Popen")
    def test_get_resume_token_success(self, mock_popen):
        """Test successful resume token retrieval."""
        from zfs.replicate.snapshot.send import _get_resume_token

        # Mock successful subprocess call
        mock_proc = mock.Mock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = (b"test_token_456\n", b"")
        mock_popen.return_value = mock_proc

        # Create test objects
        remote = filesystem("test_remote")
        current = Snapshot(filesystem("test_local"), "test_snap", None, 0)
        ssh_command = "ssh user@host"

        # Call the function
        result = _get_resume_token(remote, current, ssh_command)

        # Verify the result
        assert result == "test_token_456"

    @mock.patch("zfs.replicate.snapshot.send.subprocess.Popen")
    def test_get_resume_token_no_token(self, mock_popen):
        """Test resume token retrieval when no token exists."""
        from zfs.replicate.snapshot.send import _get_resume_token

        # Mock subprocess call returning "-" (no token)
        mock_proc = mock.Mock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = (b"-\n", b"")
        mock_popen.return_value = mock_proc

        # Create test objects
        remote = filesystem("test_remote")
        current = Snapshot(filesystem("test_local"), "test_snap", None, 0)
        ssh_command = "ssh user@host"

        # Call the function
        result = _get_resume_token(remote, current, ssh_command)

        # Verify no token is returned
        assert result is None

    @mock.patch("zfs.replicate.snapshot.send.subprocess.Popen")
    def test_get_resume_token_command_failure(self, mock_popen):
        """Test resume token retrieval when command fails."""
        from zfs.replicate.snapshot.send import _get_resume_token

        # Mock failed subprocess call
        mock_proc = mock.Mock()
        mock_proc.returncode = 1
        mock_proc.communicate.return_value = (b"", b"command failed")
        mock_popen.return_value = mock_proc

        # Create test objects
        remote = filesystem("test_remote")
        current = Snapshot(filesystem("test_local"), "test_snap", None, 0)
        ssh_command = "ssh user@host"

        # Call the function
        result = _get_resume_token(remote, current, ssh_command)

        # Verify no token is returned
        assert result is None
