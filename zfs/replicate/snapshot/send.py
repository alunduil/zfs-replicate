"""ZFS Snapshot Send."""

import subprocess  # nosec
from typing import Optional

from .. import compress, receive
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from ..receive import ReceiveOptions
from .type import Snapshot


def send(  # pylint: disable=R0917,R0913,R0914
    remote: FileSystem,
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    follow_delete: bool,
    raw: bool,
    receive_options: ReceiveOptions,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""
    send_command = _send(current, previous, follow_delete=follow_delete, raw=raw)

    compress_command, decompress_command = compress.command(compression)

    receive_command = (
        compress_command
        + ssh_command
        + " "
        + f'"{receive.command(remote, current, decompress_command, receive_options)}"'
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
    current: Snapshot,
    previous: Optional[Snapshot] = None,
    follow_delete: bool = False,
    raw: bool = True,
) -> str:
    options = []

    if raw:
        options.append("--raw")

    if follow_delete:
        options.append("-p")

    if previous is not None:
        options.append(f"-i '{previous.filesystem.name}@{previous.name}'")

    return f"/usr/bin/env - zfs send {' '.join(options)} '{current.filesystem.name}@{current.name}'"
