"""Fixtures shared across the suite."""

import pytest

from zfs.replicate.command import Command


@pytest.fixture
def ssh_command() -> Command:
    """Build the ssh invocation that carries a command to the remote host."""
    return Command.with_empty_env("ssh", "host")
