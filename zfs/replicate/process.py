"""Run a :class:`~zfs.replicate.command.Command` as a process.

Wraps stdlib ``subprocess`` so a command is exec'd from its argv list with
``shell=False`` -- arguments reach the program verbatim, never re-parsed by a
local shell. This is the one place the project spawns a process, so the
shell-free guarantee (and its bandit suppression) lives here and nowhere else.
"""

import subprocess  # nosec B404 -- the sole audited process boundary; see the Popen note below
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
    # nosec B603 -- argv list with shell=False; program names are literals and
    # untrusted data only ever rides as argv tokens, so no shell can interpret it.
    return subprocess.Popen(  # nosec B603
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
