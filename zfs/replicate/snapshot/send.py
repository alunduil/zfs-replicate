"""ZFS Snapshot Send."""

import subprocess  # nosec
from typing import Optional

from .. import compress, filesystem, receive
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from ..receive.command import command
from ..send import Options as SendOptions
from .type import Snapshot


# Threads the whole replication surface and assembles the pipeline from
# several command-string locals, so the parameter and local counts cross
# pylint's thresholds.
def send(  # pylint: disable=R0917,R0913,R0914
    remote: FileSystem,
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    send_options: SendOptions,
    receive_options: receive.Options,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""
    send_command = _send(current, previous, options=send_options)

    compress_command, decompress_command = compress.command(compression)

    destination = filesystem.remote_dataset(remote, current.filesystem)

    remote_command = decompress_command + command(destination, receive_options)

    receive_command = compress_command + ssh_command + " " + f'"{remote_command}"'

    pipeline = send_command + " | " + receive_command

    proc = subprocess.Popen(  # pylint: disable=R1732
        pipeline, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE  # nosec
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
    *,
    options: SendOptions,
) -> str:
    flags = options.to_flags()

    if previous is not None:
        flags.append(f"-i '{previous.filesystem.name}@{previous.name}'")

    return f"/usr/bin/env - zfs send {' '.join(flags)} '{current.filesystem.name}@{current.name}'"
