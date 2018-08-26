"""ZFS Snapshot Send."""


def send(
    current: Snapshot,
    ssh_command: str(),
    previous: Optional[Snapshot] = None,
    compression: Compression = Compression.LZ4,
    follow_delete: bool = False,
):
    """Send ZFS Snapshot."""

    send_command = _send(current=current, previous=previous, follow_delete=follow_delete)
    print(f"send_command: {send_command}")

    read, write = os.pipe()
    proc_pid = os.fork()
    if proc_pid == 0:
        os.close(read)
        os.dup2(write, 1)
        os.close(write)
        os.execv(send_command.split(" ")[0], send_command.split(" ")[1:])
    else:
        os.close(write)

    compress, decompress = compress.commands(compression)

    assert compress[-1] == "|"

    receive_command = " ".join([compress, ssh_command, f"'{_receive(current, decompress)}'"])

    read_fd = os.fdopen(read, "rb", 0)

    try:
        proc = subprocess.open(receive_command, shell=True, stdin=read_fd, stderr=subprocess.STDIN)
        output, _ = proc.communicate()
        os.waitpid(proc_pid, os.WNOHANG)
    finally:
        read_fd.close()

    if not ("Succeeded" in output or "failed to create mountpoint" in output):
        raise RuntimeError(f"failed to create snapshot: {current.name}")


def _send(current: Snapshot, previous: Optional[Snapshot] = None, follow_delete: bool = False) -> str:
    options = ["-V"]

    if followdelete:
        options.append("-p")

    if previous is not None:
        options.append(f"-i '{previous.dataset}@{previous.name}'")

    return f"/usr/bin/env zfs send {' '.join(options)} '{current.dataset}@{current.name}'"


def _receive(current: Snapshot, decompress: str) -> str:
    return f"{decompress} /usr/bin/env zfs receive -F -d '{current.name}'"
