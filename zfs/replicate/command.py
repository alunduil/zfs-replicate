"""Structured subprocess commands.

A :class:`Command` is an argv list -- a program plus its arguments -- rather
than a shell string. Callers build one with :meth:`Command.with_empty_env`
and hand it to ``subprocess.open``/``subprocess.run``, which exec it with
``shell=False`` so dataset names, hostnames, and options are never re-parsed
by a local shell.

The one shell that cannot be avoided is the *remote* one: ``ssh host a b c``
joins its arguments with spaces and runs the result through the remote login
shell. :func:`over_ssh` renders the wrapped command with :func:`shlex.join`, so
the remote side is quoted correctly for that shell instead of relying on
hand-placed quotes.
"""

import shlex
from dataclasses import dataclass, field
from typing import List, Mapping, Optional, Tuple

# Prefix that empties the environment before exec, kept in one place.
ENV: Tuple[str, str] = ("/usr/bin/env", "-")


@dataclass(frozen=True)
class Command:
    """A program and its arguments, executed without a shell.

    ``env`` maps to ``subprocess.Popen(env=...)`` for the local process, but
    these commands empty the environment with the :data:`ENV` prefix (via
    :meth:`with_empty_env`) instead -- that prefix also renders into the remote
    string, where no ``Popen(env=...)`` exists -- so ``env`` stays ``None``
    unless a caller overrides the child environment directly.
    """

    program: str
    args: List[str] = field(default_factory=list)
    env: Optional[Mapping[str, str]] = None

    @classmethod
    def with_empty_env(cls, program: str, *args: str) -> "Command":
        """Build a command that runs ``program`` with an emptied environment.

        Prepends the :data:`ENV` prefix (``/usr/bin/env -``) here, so no call
        site repeats it.

        >>> Command.with_empty_env("zfs", "list", "-H").argv
        ['/usr/bin/env', '-', 'zfs', 'list', '-H']
        """
        return cls(program=ENV[0], args=[ENV[1], program, *args])

    @property
    def argv(self) -> List[str]:
        """The command as one list, the form the exec layer and ``render`` consume."""
        return [self.program, *self.args]

    def render(self) -> str:
        """Render as a single shell-safe string for a remote shell.

        >>> Command("zfs", ["destroy", "pool/a b$c"]).render()
        "zfs destroy 'pool/a b$c'"
        """
        return shlex.join(self.argv)


def over_ssh(ssh_command: Command, *commands: Command) -> Command:
    """Wrap ``commands`` to run over ssh through ``ssh_command``.

    Multiple commands join with ``" | "`` into one remote pipeline (e.g. a
    decompress feeding ``zfs receive``), rendered shell-safe and handed to ssh
    as a single argument so the local exec stays shell-free.
    """
    pipeline = " | ".join(cmd.render() for cmd in commands)

    return Command(ssh_command.program, [*ssh_command.args, pipeline], ssh_command.env)
