"""ZFS Snapshot Send."""
import subprocess  # nosec
from typing import Optional

from .. import compress, filesystem
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from .type import Snapshot


def send(  # pylint: disable=R0913
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

        raise ZFSReplicateError(
            f"failed to create snapshot: '{current.filesystem.name}@{current.name}': {error!r}",
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
    return f"{decompress_command}/usr/bin/env - zfs receive -F -d '{destination.name}'"
