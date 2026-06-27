"""zfs.replicate.cli.main tests."""

from typing import Any, Dict, List

import pytest
from click.testing import CliRunner

import zfs.replicate.cli.main as sut
from zfs.replicate import receive
from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.snapshot.send import _send
from zfs.replicate.snapshot.type import Snapshot


def test_invokes_without_stacktrace() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate -l alunduil -i mypy.ini example.com bogus bogus
    """
    runner = CliRunner()
    result = runner.invoke(
        sut.main, ["-l", "alunduil", "-i", "mypy.ini", "example.com", "bogus", "bogus"]
    )
    assert isinstance(result.exception, SystemExit) or (  # nosec
        isinstance(result.exception, FileNotFoundError)
        and result.exception.filename == "/usr/bin/env"
    ), "Expected SystemExit or FileNotFoundError."


def test_no_raw_threads_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """`--no-raw` reaches task.execute and the resulting send command omits --raw.

    .. code:: bash

        zfs-replicate --no-raw -l alunduil -i mypy.ini example.com bogus bogus
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
            "--no-raw",
            "-l",
            "alunduil",
            "-i",
            "mypy.ini",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code == 0, result.output  # nosec
    assert captured.get("raw") is False  # nosec

    snapshot = Snapshot(
        filesystem=filesystem("pool/data"), name="snap", previous=None, timestamp=0
    )
    assert "--raw" not in _send(snapshot, raw=captured["raw"])  # nosec


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
            "mypy.ini",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code == 0, result.output  # nosec

    assert captured.get("receive_options") == receive.Options(  # nosec
        force=False, no_mount=True, resume=True, properties={"readonly": "on"}
    )


def test_set_rejects_malformed_property() -> None:
    """`--receive-set` without an equals sign is rejected before execution.

    .. code:: bash

        zfs-replicate --receive-set readonly -l alunduil -i mypy.ini example.com bogus bogus
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
            "mypy.ini",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert result.exit_code != 0  # nosec
    assert "KEY=VALUE" in result.output  # nosec


def test_invokes_without_stacktrace_verbose() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate --verbose -l alunduil -i mypy.ini example.com bogus bogus.
    """
    runner = CliRunner()
    result = runner.invoke(
        sut.main,
        [
            "--verbose",
            "-l",
            "alunduil",
            "-i",
            "mypy.ini",
            "example.com",
            "bogus",
            "bogus",
        ],
    )
    assert isinstance(result.exception, SystemExit) or (  # nosec
        isinstance(result.exception, FileNotFoundError)
        and result.exception.filename == "/usr/bin/env"
    ), "Expected SystemExit or FileNotFoundError."
