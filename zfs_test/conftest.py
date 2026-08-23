"""Fixtures shared across the suite."""

import subprocess
from typing import Callable

import pytest
from pytest_mock import MockerFixture

from zfs.replicate import process
from zfs.replicate.command import Command


@pytest.fixture
def ssh_command() -> Command:
    """Build the ssh invocation that carries a command to the remote host."""
    return Command.with_empty_env("ssh", "host")


@pytest.fixture
def fails_with(mocker: MockerFixture) -> Callable[[bytes], None]:
    """Fail the next run at the process boundary with the given stderr."""

    def _fails_with(error: bytes) -> None:
        mocker.patch.object(process, "run", return_value=subprocess.CompletedProcess([], 1, b"", error))

    return _fails_with
