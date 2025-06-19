"""ZFS Snapshot Send."""

import subprocess  # nosec
from typing import Optional

from .. import compress, filesystem
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from .type import Snapshot


def send(  # pylint: disable=R0917,R0913
    remote: FileSystem,
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    follow_delete: bool,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""
    send_command = _send(current, previous, follow_delete=follow_delete)

    compress_command, decompress_command = compress.command(compression)

    receive_command = (
        compress_command
        + ssh_command
        + " "
        + f'"{_receive(remote, current, decompress_command)}"'
    )

    command = send_command + " | " + receive_command

    proc = subprocess.Popen(  # pylint: disable=R1732
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE  # nosec
    )
    output, error = proc.communicate()
    output = (
        output.strip(b"\n").strip(b"\r").replace(b"WARNING: ENABLED NONE CIPHER", b"")
    )

    if proc.returncode:
        if b"failed to create mountpoint" in error:
            return  # Ignore this error.

        # Check if this is a resumable failure
        if _is_resumable_failure(error):
            _handle_resume(
                remote, current, ssh_command, compression, decompress_command
            )
            return

        raise ZFSReplicateError(
            f"failed to create snapshot: '{current.filesystem.name}@{current.name}': {error!r}",
            current,
            error,
        )


def _is_resumable_failure(error: bytes) -> bool:
    """Check if the error indicates a resumable receive failure."""
    resumable_indicators = [
        b"cannot receive new filesystem stream",
        b"resumable receive",
        b"incomplete stream",
        b"connection reset",
        b"broken pipe",
    ]
    return any(indicator in error.lower() for indicator in resumable_indicators)


def _get_resume_token(
    remote: FileSystem, current: Snapshot, ssh_command: str
) -> Optional[str]:
    """Get the resume token from the remote filesystem."""
    destination = filesystem.remote_dataset(remote, current.filesystem)

    command = f"{ssh_command} \"zfs get -H -o value receive_resume_token '{destination.name}'\""

    proc = subprocess.Popen(  # pylint: disable=R1732
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE  # nosec
    )
    output, error = proc.communicate()

    if proc.returncode == 0:
        token = output.decode().strip()
        if token and token != "-":
            return token

    return None


def _send_resume(token: str) -> str:
    """Create a send command using a resume token."""
    return f"/usr/bin/env - zfs send -w -t '{token}'"


def _handle_resume(
    remote: FileSystem,
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    decompress_command: str,
) -> None:
    """Handle resuming an interrupted send/receive."""
    token = _get_resume_token(remote, current, ssh_command)

    if not token:
        # No resume token available, re-raise the original error
        raise ZFSReplicateError(
            f"Failed to send snapshot and no resume token available: '{current.filesystem.name}@{current.name}'",
            current,
            b"No resume token",
        )

    # Resume the send using the token
    send_command = _send_resume(token)
    destination = filesystem.remote_dataset(remote, current.filesystem)

    compress_command, _ = compress.command(compression)

    receive_command = (
        compress_command
        + ssh_command
        + " "
        + f"\"{decompress_command}/usr/bin/env - zfs receive -s -F '{destination.name}'\""
    )

    command = send_command + " | " + receive_command

    proc = subprocess.Popen(  # pylint: disable=R1732
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE  # nosec
    )
    output, error = proc.communicate()

    if proc.returncode:
        raise ZFSReplicateError(
            f"failed to resume snapshot: '{current.filesystem.name}@{current.name}': {error!r}",
            current,
            error,
        )


def _send(
    current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False
) -> str:
    options = ["--raw"]  # ["-V"]

    if follow_delete:
        options.append("-p")

    if previous is not None:
        options.append(f"-i '{previous.filesystem.name}@{previous.name}'")

    return f"/usr/bin/env - zfs send {' '.join(options)} '{current.filesystem.name}@{current.name}'"


def _receive(remote: FileSystem, current: Snapshot, decompress_command: str) -> str:
    destination = filesystem.remote_dataset(remote, current.filesystem)
    return (
        f"{decompress_command}/usr/bin/env - zfs receive -s -F -d '{destination.name}'"
    )
