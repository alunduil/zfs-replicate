"""ZFS Snapshot Send."""

from dataclasses import dataclass
from typing import IO, List, Optional

from .. import compress, filesystem, process, receive
from ..command import Command, over_ssh
from ..compress import Compression
from ..error import ZFSReplicateError
from ..filesystem import FileSystem
from ..receive.command import command as receive_command
from ..send import Options as SendOptions
from .type import Snapshot

_MOUNTPOINT_FAILURE = b"failed to create mountpoint"


@dataclass(frozen=True)
class Pipeline:
    """The stages of ``send [ | compress ] | ssh "[decompress | ] receive"``.

    ``receive`` covers the whole remote side, ssh included; ``compress`` is
    ``None`` when compression is off.
    """

    send: Command
    compress: Optional[Command]
    receive: Command


def send(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    current: Snapshot,
    *,
    ssh_command: Command,
    compression: Compression,
    send_options: SendOptions,
    receive_options: receive.Options,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""
    proc = _spawn(
        _pipeline(
            remote,
            current,
            ssh_command=ssh_command,
            compression=compression,
            send_options=send_options,
            receive_options=receive_options,
            previous=previous,
        )
    )

    _, error = proc.communicate()

    _raise_for_failure(current, proc.returncode, error)


def _pipeline(  # noqa: PLR0913 -- carries the full replication call surface
    remote: FileSystem,
    current: Snapshot,
    *,
    ssh_command: Command,
    compression: Compression,
    send_options: SendOptions,
    receive_options: receive.Options,
    previous: Optional[Snapshot] = None,
) -> Pipeline:
    """Assemble the pipeline that replicates ``current`` onto ``remote``, spawning nothing."""
    compress_command, decompress_command = compress.command(compression)

    destination = filesystem.remote_dataset(remote, current.filesystem)

    remote_side: List[Command] = [
        cmd for cmd in (decompress_command, receive_command(destination, receive_options)) if cmd is not None
    ]

    return Pipeline(
        send=_send(current, previous, options=send_options),
        compress=compress_command,
        receive=over_ssh(ssh_command, *remote_side),
    )


def _spawn(pipeline: Pipeline) -> "process.Popen[bytes]":
    """Run a :class:`Pipeline` as local processes without a shell.

    Only the receive side (over ssh) runs through a shell -- the remote one,
    which ssh cannot avoid. Each upstream stage keeps its own stderr on the
    parent's, so send/compress errors stay visible; the ssh stage's streams are
    captured for the caller's error handling.
    """
    upstream = process.open(pipeline.send, stdin=process.DEVNULL, stdout=process.PIPE, stderr=None)

    if pipeline.compress is not None:
        compressor = process.open(
            pipeline.compress,
            stdin=upstream.stdout,
            stdout=process.PIPE,
            stderr=None,
        )
        _detach(upstream.stdout)
        upstream = compressor

    proc = process.open(
        pipeline.receive,
        stdin=upstream.stdout,
        stdout=process.PIPE,
        stderr=process.PIPE,
    )
    _detach(upstream.stdout)

    return proc


def _raise_for_failure(current: Snapshot, returncode: int, error: bytes) -> None:
    """Raise unless the pipeline succeeded or failed only to create the mountpoint."""
    if not returncode or _MOUNTPOINT_FAILURE in error:
        return

    raise ZFSReplicateError(
        f"failed to create snapshot: '{current.filesystem.name}@{current.name}': {error!r}",
        current,
        error,
    )


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
