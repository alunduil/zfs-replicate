"""ZFS Snapshot Send."""

from dataclasses import dataclass
from typing import List, Optional

from .. import compress, filesystem, optional, process, receive
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

    @property
    def stages(self) -> List[Command]:
        """The commands to run, in pipeline order."""
        return optional.values(self.send, self.compress, self.receive)


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
    pipeline = _pipeline(
        remote,
        current,
        ssh_command=ssh_command,
        compression=compression,
        send_options=send_options,
        receive_options=receive_options,
        previous=previous,
    )

    proc = process.pipeline(*pipeline.stages)

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
    compression_commands = compress.command(compression)

    compress_command: Optional[Command] = None
    decompress_command: Optional[Command] = None

    if compression_commands is not None:
        compress_command = compression_commands.compress
        decompress_command = compression_commands.decompress

    destination = filesystem.remote_dataset(remote, current.filesystem)

    remote_side = optional.values(decompress_command, receive_command(destination, receive_options))

    return Pipeline(
        send=_send(current, previous, options=send_options),
        compress=compress_command,
        receive=over_ssh(ssh_command, *remote_side),
    )


def _raise_for_failure(current: Snapshot, returncode: int, error: bytes) -> None:
    """Raise unless the pipeline succeeded or failed only to create the mountpoint."""
    if not returncode or _MOUNTPOINT_FAILURE in error:
        return

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
) -> Command:
    flags = options.to_flags()

    if previous is not None:
        flags.extend(["-i", f"{previous.filesystem.name}@{previous.name}"])

    return Command.with_empty_env("zfs", "send", *flags, f"{current.filesystem.name}@{current.name}")
