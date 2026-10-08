"""Task Run Context Values."""

from zfs.replicate import receive, send
from zfs.replicate.command import Command
from zfs.replicate.compress import Compression
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.task.context import RunContext

CONTEXT = RunContext(
    remote=filesystem("backup"),
    ssh_command=Command("ssh", ["backup.example.com"]),
    compression=Compression.LZ4,
    send_options=send.Options(large_block=False, raw=True, embed=False, compressed=False, props=False),
    receive_options=receive.Options(force=True, no_mount=False, resume=False, properties={}),
)
