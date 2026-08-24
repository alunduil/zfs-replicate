"""Run a :class:`~zfs.replicate.command.Command` as a process.

Wraps stdlib ``subprocess`` so a command is exec'd from its argv list with
``shell=False`` -- arguments reach the program verbatim, never re-parsed by a
local shell. This is the one place the project spawns a process, so the
shell-free guarantee lives here and nowhere else.
"""

import subprocess
from typing import IO

from .command import Command

STDOUT = subprocess.STDOUT
PIPE = subprocess.PIPE
DEVNULL = subprocess.DEVNULL
Popen = subprocess.Popen

# None means "inherit the parent's stream"; an int is a file descriptor or one
# of PIPE/DEVNULL/STDOUT; an IO wires one process's stream to another's.
Stream = IO[bytes] | int | None


def open(
    command: Command,
    stdin: Stream = subprocess.PIPE,
    stdout: Stream = subprocess.PIPE,
    stderr: Stream = subprocess.PIPE,
) -> "subprocess.Popen[bytes]":
    """Start ``command`` as a process, for streaming or pipeline wiring."""
    # argv list with shell=False; program names are literals and untrusted data
    # only ever rides as argv tokens, so no shell can interpret it.
    return subprocess.Popen(  # noqa: S603
        command.argv,
        env=command.env,
        stdin=stdin,
        stdout=stdout,
        stderr=stderr,
    )


def pipeline(first: Command, *rest: Command) -> "subprocess.Popen[bytes]":
    """Chain ``first`` into each of ``rest``, returning the last stage.

    The "Replacing shell pipeline" recipe from the ``subprocess`` documentation,
    generalised over any number of stages.

    Every stage but the last keeps the parent's stderr, so its failures stay
    visible; the last stage captures both streams for the caller to read.
    """
    proc = open(first, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=None if rest else subprocess.PIPE)

    for index, command in enumerate(rest):
        upstream = proc
        stderr = subprocess.PIPE if index == len(rest) - 1 else None

        proc = open(command, stdin=upstream.stdout, stdout=subprocess.PIPE, stderr=stderr)

        _detach(upstream.stdout)

    return proc


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


def _detach(stream: IO[bytes] | None) -> None:
    """Drop the parent's copy of a piped stream so its reader sees EOF/SIGPIPE."""
    if stream is not None:
        stream.close()
