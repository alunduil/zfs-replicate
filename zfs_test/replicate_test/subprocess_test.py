"""Test the shell-free subprocess wrapper."""

import zfs.replicate.subprocess as sut
from zfs.replicate.command import Command


def test_run_passes_arguments_without_a_shell() -> None:
    """An argument with shell syntax reaches the program verbatim, unexpanded."""
    hostile = "$(echo pwned) `id` ; rm -rf"

    result = sut.run(Command("printf", ["%s", hostile]))

    assert result.returncode == 0  # nosec
    assert result.stdout == hostile.encode()  # nosec


def test_run_reports_nonzero_returncode() -> None:
    """A failing program surfaces its exit status on the result."""
    result = sut.run(Command("false", []))

    assert result.returncode != 0  # nosec
