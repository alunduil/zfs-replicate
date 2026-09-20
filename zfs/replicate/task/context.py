"""Task Run Context."""

from dataclasses import dataclass

from .. import receive, send
from ..command import Command
from ..compress import Compression
from ..filesystem import FileSystem


@dataclass(frozen=True)
class RunContext:
    """What every task needs beyond itself to reach the remote."""

    remote: FileSystem
    ssh_command: Command
    compression: Compression
    send_options: send.Options
    receive_options: receive.Options
