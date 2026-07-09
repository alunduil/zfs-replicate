"""zfs.replicate.cli.main tests."""

from typing import Any, Dict, List

import pytest
from click.testing import CliRunner

import zfs.replicate.cli.main as sut
from zfs.replicate import receive, send
from zfs.replicate.snapshot.type import Snapshot


def test_invokes_without_stacktrace() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate -l alunduil -i pyproject.toml example.com bogus bogus
    """
    runner = CliRunner()
    result = runner.invoke(sut.main, ["-l", "alunduil", "-i", "pyproject.toml", "example.com", "bogus", "bogus"])
    assert isinstance(result.exception, SystemExit) or (
        isinstance(result.exception, FileNotFoundError) and result.exception.filename == "/usr/bin/env"
    ), "Expected SystemExit or FileNotFoundError."


def test_send_options_thread_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """Send flags reach task.execute as the expected send.Options.

    .. code:: bash

        zfs-replicate --send-no-raw --send-large-block --send-embed --send-compressed --send-props ...
    """
    captured: Dict[str, Any] = {}

    def fake_list(*_args: Any, **_kwargs: Any) -> List[Snapshot]:
        return []

    def fake_create(*_args: Any, **_kwargs: Any) -> None:
        return None

    def fake_execute(*_args: Any, **kwargs: Any) -> None:
        captured.update(kwargs)

    monkeypatch.setattr("zfs.replicate.cli.main.snapshot.list", fake_list)
    monkeypatch.setattr("zfs.replicate.cli.main.filesystem.create", fake_create)
    monkeypatch.setattr("zfs.replicate.cli.main.task.execute", fake_execute)

    runner = CliRunner()
    result = runner.invoke(
        sut.main,
        [
            "--send-no-raw",
            "--send-large-block",
            "--send-embed",
            "--send-compressed",
            "--send-props",
            "-l",
            "alunduil",
            "-i",
            "pyproject.toml",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code == 0, result.output

    assert captured.get("send_options") == send.Options(
        large_block=True, raw=False, embed=True, compressed=True, props=True
    )


def test_receive_options_thread_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """Receive flags reach task.execute and shape the receive command.

    .. code:: bash

        zfs-replicate --receive-no-force --receive-no-mount --receive-resume-token-capable --receive-set readonly=on ...
    """
    captured: Dict[str, Any] = {}

    def fake_list(*_args: Any, **_kwargs: Any) -> List[Snapshot]:
        return []

    def fake_create(*_args: Any, **_kwargs: Any) -> None:
        return None

    def fake_execute(*_args: Any, **kwargs: Any) -> None:
        captured.update(kwargs)

    monkeypatch.setattr("zfs.replicate.cli.main.snapshot.list", fake_list)
    monkeypatch.setattr("zfs.replicate.cli.main.filesystem.create", fake_create)
    monkeypatch.setattr("zfs.replicate.cli.main.task.execute", fake_execute)

    runner = CliRunner()
    result = runner.invoke(
        sut.main,
        [
            "--receive-no-force",
            "--receive-no-mount",
            "--receive-resume-token-capable",
            "--receive-set",
            "readonly=on",
            "-l",
            "alunduil",
            "-i",
            "pyproject.toml",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code == 0, result.output

    assert captured.get("receive_options") == receive.Options(
        force=False, no_mount=True, resume=True, properties={"readonly": "on"}
    )


def test_set_rejects_malformed_property() -> None:
    """`--receive-set` without an equals sign is rejected before execution.

    .. code:: bash

        zfs-replicate --receive-set readonly -l alunduil -i pyproject.toml example.com bogus bogus
    """
    runner = CliRunner()
    result = runner.invoke(
        sut.main,
        [
            "--receive-set",
            "readonly",
            "-l",
            "alunduil",
            "-i",
            "pyproject.toml",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code != 0
    assert "KEY=VALUE" in result.output
