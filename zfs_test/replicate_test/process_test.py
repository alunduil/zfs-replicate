"""Test the shell-free process wrapper."""

import zfs.replicate.process as sut
from zfs.replicate.command import Command


def test_run_passes_arguments_without_a_shell() -> None:
    """An argument with shell syntax reaches the program verbatim, unexpanded."""
    hostile = "$(echo pwned) `id` ; rm -rf"

    result = sut.run(Command("printf", ["%s", hostile]))

    assert result.returncode == 0
    assert result.stdout == hostile.encode()


def test_run_reports_nonzero_returncode() -> None:
    """A failing program surfaces its exit status on the result."""
    result = sut.run(Command("false", []))

    assert result.returncode != 0


def test_pipeline_feeds_each_stage_into_the_next() -> None:
    """A stage's stdout arrives on the next stage's stdin."""
    proc = sut.pipeline(Command("printf", ["%s", "replicate"]), Command("tr", ["a-z", "A-Z"]))

    output, _ = proc.communicate()

    assert output == b"REPLICATE"


def test_pipeline_captures_both_streams_of_a_lone_stage() -> None:
    """A one-stage pipeline is its own last stage, so the caller reads its streams."""
    proc = sut.pipeline(Command("printf", ["%s", "solo"]))

    output, error = proc.communicate()

    assert output == b"solo"
    assert error == b""
