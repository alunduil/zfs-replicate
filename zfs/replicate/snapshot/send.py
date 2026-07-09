"""ZFS Snapshot Send."""

from typing import IO, List, Optional

from .. import compress, filesystem, process, receive
from ..command import Command, over_ssh
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from ..receive.command import command as receive_command
from ..send import Options as SendOptions
from .type import Snapshot


# Threads the whole replication surface to assemble the send pipeline, so the
# parameter count crosses pylint's threshold.
def send(  # pylint: disable=R0917,R0913
    remote: FileSystem,
    current: Snapshot,
    ssh_command: Command,
    compression: Compression,
    send_options: SendOptions,
    receive_options: receive.Options,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""
    compress_command, decompress_command = compress.command(compression)

    destination = filesystem.remote_dataset(remote, current.filesystem)

    remote_side: List[Command] = [
        cmd for cmd in (decompress_command, receive_command(destination, receive_options)) if cmd is not None
    ]

    proc = _pipeline(
        _send(current, previous, options=send_options),
        compress_command,
        over_ssh(ssh_command, *remote_side),
    )

    _, error = proc.communicate()

    if proc.returncode:
        if b"failed to create mountpoint" in error:
            return  # Ignore this error.

        raise ZFSReplicateError(
            f"failed to create snapshot: '{current.filesystem.name}@{current.name}': {error!r}",
            current,
            error,
        )


def _pipeline(
    send_command: Command,
    compress_command: Optional[Command],
    remote_command: Command,
) -> "process.Popen[bytes]":
    """Wire ``send [ | compress ] | ssh`` as local processes without a shell.

    Only the receive side (over ssh) runs through a shell -- the remote one,
    which ssh cannot avoid. Each upstream stage keeps its own stderr on the
    parent's, so send/compress errors stay visible; the ssh stage's streams are
    captured for the caller's error handling.
    """
    upstream = process.open(send_command, stdin=process.DEVNULL, stdout=process.PIPE, stderr=None)

    if compress_command is not None:
        compressor = process.open(
            compress_command,
            stdin=upstream.stdout,
            stdout=process.PIPE,
            stderr=None,
        )
        _detach(upstream.stdout)
        upstream = compressor

    proc = process.open(
        remote_command,
        stdin=upstream.stdout,
        stdout=process.PIPE,
        stderr=process.PIPE,
    )
    _detach(upstream.stdout)

    return proc


def _detach(stream: Optional[IO[bytes]]) -> None:
    """Drop the parent's copy of a piped stream so its reader sees EOF/SIGPIPE."""
    if stream is not None:
        stream.close()


def _send(
    current: Snapshot,
    previous: Optional[Snapshot] = None,
    *,
    options: SendOptions,
) -> Command:
    flags = options.to_flags()

    if previous is not None:
        flags.extend(["-i", f"{previous.filesystem.name}@{previous.name}"])

    return Command.with_empty_env("zfs", "send", *flags, f"{current.filesystem.name}@{current.name}")
