"""ZFS Snapshot Send."""

import os
import shlex
import tempfile
from typing import Optional, Tuple

import click

from .. import compress, filesystem, subprocess
from ..compress import Compression
from ..filesystem import FileSystem
from .type import Snapshot


def send(  # pylint: disable=too-many-arguments,too-many-locals
    remote: FileSystem,
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    follow_delete: bool,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""

    send_pid, read_fd = _send(current, previous, follow_delete=follow_delete)

    compress_command, decompress_command = compress.command(compression)

    receive_command = compress_command + ssh_command + " " + f'"{_receive(remote, current, decompress_command)}"'
    click.secho(receive_command, fg="red")

    read = os.fdopen(read_fd, "rb", 0)

    with tempfile.TemporaryFile("w+") as out:
        proc = subprocess.open(receive_command, shell=True, stdin=read, stdout=out, stderr=subprocess.STDOUT)
        proc.wait()
        os.waitpid(send_pid, os.WNOHANG)
        read.close()
        out.seek(0)
        output = out.read().strip("\n").strip("\r")

    output = output.replace("WARNING: ENABLED NONE CIPHER", "")

    # click.secho(output, fg="yellow")

    if proc.returncode:
        if b"Succeeded" in output and b"failed to create mountpoint" in output:
            return  # Ignore this error.

        raise RuntimeError(f"failed to create snapshot: {current.name}: {output}")


def _send(current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False) -> Tuple[int, int]:
    send_command = _send_command(current=current, previous=previous, follow_delete=follow_delete)
    click.secho(send_command, fg="red")

    read_fd, write_fd = os.pipe()
    pid = os.fork()
    if pid == 0:
        command = shlex.split(send_command)
        os.close(read_fd)
        os.dup2(write_fd, 1)
        os.close(write_fd)

        os.execvp(command[0], command)
    else:
        os.close(write_fd)

    return pid, read_fd


def _send_command(current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False) -> str:
    options = []  # ["-V"]

    if follow_delete:
        options.append("-p")

    if previous is not None:
        options.append(f"-i '{previous.filesystem.name}@{previous.name}'")

    return f"/usr/bin/env - zfs send {' '.join(options)} '{current.filesystem.name}@{current.name}'"


def _receive(remote: FileSystem, current: Snapshot, decompress_command: str) -> str:
    destination = filesystem.remote_name(remote, current.filesystem)
    return f"{decompress_command}/usr/bin/env - zfs receive -F -d '{destination.name}'"
