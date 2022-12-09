"""subprocess wrapper."""
import shlex
import subprocess  # nosec
from typing import IO, Union

STDOUT = subprocess.STDOUT


def open(  # pylint: disable=W0622
    command: str,
    shell: bool = False,
    stdin: Union[IO[str], int] = subprocess.PIPE,
    stdout: Union[IO[str], int] = subprocess.PIPE,
    stderr: Union[IO[str], int] = subprocess.PIPE,
) -> "subprocess.Popen[bytes]":
    """Wrap subprocess.Popen for convenience."""
    return subprocess.Popen(
        shlex.split(command),
        stdin=stdin,
        stdout=stdout,
        stderr=stderr,
        shell=shell,  # nosec
    )
