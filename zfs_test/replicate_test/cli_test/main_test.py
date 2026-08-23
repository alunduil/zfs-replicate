"""zfs.replicate.cli.main tests."""

from typing import Callable

import pytest
from click.testing import CliRunner, Result
from pytest_mock import MockerFixture, MockType

import zfs.replicate.cli.main as sut
from zfs.replicate import receive, send

# Every invocation needs a destination and a key file.
CONNECTION = ["-l", "alunduil", "-i", "pyproject.toml", "example.com", "bogus", "bogus"]


class TestMain:
    """Parsed options reach ``task.execute`` intact, and bad ones stop the run."""

    @pytest.fixture
    def invoke(self) -> Callable[..., Result]:
        """Invoke the command line with the connection arguments every run needs."""

        def _invoke(*options: str) -> Result:
            return CliRunner().invoke(sut.main, [*options, *CONNECTION])

        return _invoke

    @pytest.fixture
    def execute(self, mocker: MockerFixture) -> MockType:
        """Stand in for the collaborators a run dispatches to."""
        mocker.patch("zfs.replicate.cli.main.snapshot.list", return_value=[])
        mocker.patch("zfs.replicate.cli.main.filesystem.create")

        return mocker.patch("zfs.replicate.cli.main.task.execute")

    def test_invokes_without_stacktrace(self, invoke: Callable[..., Result]) -> None:
        """Invoke without stacktrace.

        .. code:: bash

            zfs-replicate -l alunduil -i pyproject.toml example.com bogus bogus
        """
        result = invoke()

        assert isinstance(result.exception, SystemExit) or (
            isinstance(result.exception, FileNotFoundError) and result.exception.filename == "/usr/bin/env"
        ), "Expected SystemExit or FileNotFoundError."

    def test_send_options_thread_to_execute(self, invoke: Callable[..., Result], execute: MockType) -> None:
        """Send flags reach task.execute as the expected send.Options.

        .. code:: bash

            zfs-replicate --send-no-raw --send-large-block --send-embed --send-compressed --send-props ...
        """
        result = invoke(
            "--send-no-raw",
            "--send-large-block",
            "--send-embed",
            "--send-compressed",
            "--send-props",
        )
        assert result.exit_code == 0, result.output

        assert execute.call_args.kwargs["send_options"] == send.Options(
            large_block=True, raw=False, embed=True, compressed=True, props=True
        )

    def test_receive_options_thread_to_execute(self, invoke: Callable[..., Result], execute: MockType) -> None:
        """Receive flags reach task.execute and shape the receive command.

        .. code:: bash

            zfs-replicate --receive-no-force --receive-no-mount
                --receive-resume-token-capable --receive-set readonly=on ...
        """
        result = invoke(
            "--receive-no-force",
            "--receive-no-mount",
            "--receive-resume-token-capable",
            "--receive-set",
            "readonly=on",
        )
        assert result.exit_code == 0, result.output

        assert execute.call_args.kwargs["receive_options"] == receive.Options(
            force=False, no_mount=True, resume=True, properties={"readonly": "on"}
        )

    def test_set_rejects_malformed_property(self, invoke: Callable[..., Result]) -> None:
        """`--receive-set` without an equals sign is rejected before execution.

        .. code:: bash

            zfs-replicate --receive-set readonly -l alunduil -i pyproject.toml example.com bogus bogus
        """
        result = invoke("--receive-set", "readonly")

        assert result.exit_code != 0
        assert "KEY=VALUE" in result.output
