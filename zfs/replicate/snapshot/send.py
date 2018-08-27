"""ZFS Snapshot Send."""

import os
from typing import Optional, Tuple

from .. import compress, subprocess
from ..compress import Compression
from .type import Snapshot


def send(
    current: Snapshot,
    ssh_command: str,
    compression: Compression,
    follow_delete: bool,
    previous: Optional[Snapshot] = None,
) -> None:
    """Send ZFS Snapshot."""

    send_pid, read = _send(current, previous, follow_delete=follow_delete)

    compress_command, decompress_command = compress.command(compression)

    assert compress_command[-1] == "|"

    receive_command = " ".join([compress_command, ssh_command, f"'{_receive(current, decompress_command)}'"])

    read_fd = os.fdopen(read, "rb", 0)

    try:
        proc = subprocess.open(receive_command, shell=True, stdin=read_fd, stderr=subprocess.STDOUT)
        output, _ = proc.communicate()
        os.waitpid(send_pid, os.WNOHANG)
    finally:
        read_fd.close()

    if not ("Succeeded" in output or "failed to create mountpoint" in output):
        raise RuntimeError(f"failed to create snapshot: {current.name}")


def _send(current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False) -> Tuple[int, int]:
    send_command = _send_command(current=current, previous=previous, follow_delete=follow_delete)
    print(f"send_command: {send_command}")

    read, write = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(read)
        os.dup2(write, 1)
        os.close(write)
        os.execv(send_command.split(" ")[0], send_command.split(" ")[1:])
    else:
        os.close(write)

    return pid, read


def _send_command(current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False) -> str:
    options = ["-V"]

    if follow_delete:
        options.append("-p")

    if previous is not None:
        options.append(f"-i '{previous.filesystem}@{previous.name}'")

    return f"/usr/bin/env zfs send {' '.join(options)} '{current.filesystem}@{current.name}'"


def _receive(current: Snapshot, decompress_command: str) -> str:
    return f"{decompress_command} /usr/bin/env zfs receive -F -d '{current.name}'"
