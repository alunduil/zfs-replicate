"""subprocess wrapper.

Runs a :class:`~zfs.replicate.command.Command` (an argv list) with
``shell=False``, so arguments are passed to the program verbatim and never
re-parsed by a local shell.
"""

import subprocess
from typing import IO, Optional, Union

from .command import Command

STDOUT = subprocess.STDOUT
PIPE = subprocess.PIPE
DEVNULL = subprocess.DEVNULL
Popen = subprocess.Popen

# None means "inherit the parent's stream"; an int is a file descriptor or one
# of PIPE/DEVNULL/STDOUT; an IO wires one process's stream to another's.
Stream = Optional[Union[IO[bytes], int]]


def open(  # pylint: disable=W0622
    command: Command,
    stdin: Stream = subprocess.PIPE,
    stdout: Stream = subprocess.PIPE,
    stderr: Stream = subprocess.PIPE,
) -> "subprocess.Popen[bytes]":
    """Start ``command`` as a process, for streaming or pipeline wiring."""
    return subprocess.Popen(
        command.argv,
        env=command.env,
        stdin=stdin,
        stdout=stdout,
        stderr=stderr,
    )


def run(
    command: Command,
    stdin: Stream = subprocess.PIPE,
    stdout: Stream = subprocess.PIPE,
    stderr: Stream = subprocess.PIPE,
) -> "subprocess.CompletedProcess[bytes]":
    """Run ``command`` to completion and return its captured result."""
    with open(command, stdin=stdin, stdout=stdout, stderr=stderr) as proc:
        output, error = proc.communicate()

    return subprocess.CompletedProcess(command.argv, proc.returncode, output, error)
