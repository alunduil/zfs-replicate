"""subprocess wrapper."""

import shlex
import subprocess

STDOUT = subprocess.STDOUT


def open(  # pylint: disable=redefined-builtin
    command: str,
    # close_fds: bool() = True,
    # env: Optional[int()] = None,
    shell: bool = False,
    stdin: int = subprocess.PIPE,
    stdout: int = subprocess.PIPE,
    stderr: int = subprocess.PIPE,
    # start_new_session: bool() = False,
) -> subprocess.Popen:
    """Wrapper around subprocess.Popen."""

    return subprocess.Popen(
        shlex.split(command),
        stdin=stdin,
        stdout=stdout,
        stderr=stderr,
        shell=shell,
        # close_fds=close_fds,
        # preexec_fn=preexec_fn,
        # env=env,
        # encoding="utf8",
        # start_new_session=start_new_session,
    )


# def unblock_sigchld():
#     """Remove SIGCHLD from handlers."""
#
#     pmask = signal.pthread_sigmask(signal.SIG_BLOCK, [])
#     pmask.remove(signal.SIGCHLD)
#     signal.pthread_sigmask(signal.SIG_SETMASK, pmask)
