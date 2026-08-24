"""Fixtures shared across the suite."""

import os
import subprocess
from typing import Callable

import pytest
from hypothesis import HealthCheck, settings
from pytest_mock import MockerFixture

from zfs.replicate import process
from zfs.replicate.command import Command

# mutmut runs the suite several times in one process---stats, clean tests, then
# a pass per mutant---and pytest builds a fresh instance of a test class for
# each pass. Hypothesis reads those instances as differing executors and fails
# the health check on the second pass. The classes are namespaces that hold no
# state between passes, so the check has nothing to catch under mutmut; an
# ordinary pytest run, where it could catch something, keeps it on.
if "MUTANT_UNDER_TEST" in os.environ:
    settings.register_profile("mutmut", suppress_health_check=[HealthCheck.differing_executors])
    settings.load_profile("mutmut")


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
