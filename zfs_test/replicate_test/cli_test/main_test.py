"""zfs.replicate.cli.main tests."""

from typing import Any, Dict, List

import pytest
from click.testing import CliRunner, Result

import zfs.replicate.cli.main as sut
from zfs.replicate import receive, send
from zfs.replicate.snapshot.type import Snapshot

# --identity-file has to exist, so it points at a file every checkout has.
CONNECTION = ["-l", "alunduil", "-i", "pyproject.toml"]
TARGET = ["example.com", "bogus", "bogus"]


def test_invokes_without_stacktrace() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate -l alunduil -i pyproject.toml example.com bogus bogus
    """
    result = _invoke()

    assert isinstance(result.exception, SystemExit) or (
        isinstance(result.exception, FileNotFoundError) and result.exception.filename == "/usr/bin/env"
    ), "Expected SystemExit or FileNotFoundError."


def test_send_options_thread_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """Send flags reach task.execute as the expected send.Options.

    .. code:: bash

        zfs-replicate --send-no-raw --send-large-block --send-embed --send-compressed --send-props ...
    """
    captured = _execute_keywords(
        monkeypatch,
        "--send-no-raw",
        "--send-large-block",
        "--send-embed",
        "--send-compressed",
        "--send-props",
    )

    assert captured.get("send_options") == send.Options(
        large_block=True, raw=False, embed=True, compressed=True, props=True
    )


def test_receive_options_thread_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """Receive flags reach task.execute and shape the receive command.

    .. code:: bash

        zfs-replicate --receive-no-force --receive-no-mount --receive-resume-token-capable --receive-set readonly=on ...
    """
    captured = _execute_keywords(
        monkeypatch,
        "--receive-no-force",
        "--receive-no-mount",
        "--receive-resume-token-capable",
        "--receive-set",
        "readonly=on",
    )

    assert captured.get("receive_options") == receive.Options(
        force=False, no_mount=True, resume=True, properties={"readonly": "on"}
    )


def test_jobs_threads_to_execute(monkeypatch: pytest.MonkeyPatch) -> None:
    """`--jobs` reaches task.execute as the bound on concurrent data sets.

    .. code:: bash

        zfs-replicate -j 4 -l alunduil -i pyproject.toml example.com bogus bogus
    """
    captured = _execute_keywords(monkeypatch, "-j", "4")

    assert captured.get("jobs") == 4


def test_set_rejects_malformed_property() -> None:
    """`--receive-set` without an equals sign is rejected before execution.

    .. code:: bash

        zfs-replicate --receive-set readonly -l alunduil -i pyproject.toml example.com bogus bogus
    """
    result = _invoke("--receive-set", "readonly")

    assert result.exit_code != 0
    assert "KEY=VALUE" in result.output


def _invoke(*args: str) -> Result:
    """Run the CLI with ``args`` ahead of the options and arguments every invocation needs."""
    return CliRunner().invoke(sut.main, [*args, *CONNECTION, *TARGET])


def _execute_keywords(monkeypatch: pytest.MonkeyPatch, *args: str) -> Dict[str, Any]:
    """Run the CLI with the ZFS calls stubbed and return the keywords task.execute got."""
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

    result = _invoke(*args)
    assert result.exit_code == 0, result.output

    return captured
