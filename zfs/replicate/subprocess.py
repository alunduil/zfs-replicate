"""subprocess wrapper."""

import shlex
import subprocess
from typing import IO, Union

STDOUT = subprocess.STDOUT


def open(  # pylint: disable=redefined-builtin
    command: str,
    shell: bool = False,
    stdin: Union[IO[str], int] = subprocess.PIPE,
    stdout: Union[IO[str], int] = subprocess.PIPE,
    stderr: Union[IO[str], int] = subprocess.PIPE,
) -> subprocess.Popen:
    """Wrapper around subprocess.Popen."""

    return subprocess.Popen(shlex.split(command), stdin=stdin, stdout=stdout, stderr=stderr, shell=shell)
