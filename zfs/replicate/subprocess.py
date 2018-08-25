"""subprocess wrapper."""

import signal
import subprocess

from typing import List, Optional

def open( # pylint: disable=redefined-builtin,too-many-arguments
        command: List[str()],
        allow_fork: bool()=False,
        close_fds: bool()=True,
        env: Optional[any()]=None,
        stdout: any()=subprocess.PIPE,
        stderr: any()=subprocess.PIPE,
        start_new_session: bool()=False,
    ) -> any():
    """Wrapper around subprocess.Popen."""

    preexec_fn = None
    if allow_fork:
        preexec_fn = unblock_sigchld

    return subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=stdout,
        stderr=stderr,
        close_fds=close_fds,
        preexec_fn=preexec_fn,
        env=env,
        encoding='utf8',
        start_new_session=start_new_session,
        )

def unblock_sigchld():
    """Remove SIGCHLD from handlers."""

    pmask = signal.pthread_sigmask(signal.SIG_BLOCK, [])
    pmask.remove(signal.SIGCHLD)
    signal.pthread_sigmask(signal.SIG_SETMASK, pmask)
