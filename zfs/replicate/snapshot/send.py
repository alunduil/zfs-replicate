def send(snapshot: Snapshot, ssh_command: str(), previous: Optional[Snapshot]=None, compression: Compression=Compression.LZ4, follow_delete: bool()=False):
    send_command = _send(previous=previous, follow_delete=follow_delete)

    read, write = os.pipe()
    proc_pid = os.fork()
    if proc_pid == 0:
        os.close(read)
        os.dup2(write, 1)
        os.close(write)
        os.execv("/usr/bin/env", send_command)
    else:
        os.close(write)

    compress, decompress = compress.commands(compression)

    receive_command = compress + " | " + ssh_command + " " + decompress + " " + _receive(snapshot)

    read_fd = os.fdopen(read, "rb", 0)

    proc = subprocess.open(
            receive_command,
            shell=True,
            stdin=read_fd,
