"""zfs.replicate.cli.main tests."""
from click.testing import CliRunner

from zfs.replicate.cli.main import main


def test_invokes_without_stacktrace() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate -l alunduil -i mypy.ini example.com bogus bogus
    """
    runner = CliRunner()
    result = runner.invoke(
        main, ["-l", "alunduil", "-i", "mypy.ini", "example.com", "bogus", "bogus"]
    )
    assert isinstance(result.exception, SystemExit) or (  # nosec
        isinstance(result.exception, FileNotFoundError)
        and result.exception.filename == "/usr/bin/env"
    ), "Expected SystemExit or FileNotFoundError."


def test_invokes_without_stacktrace_verbose() -> None:
    """Invoke without stacktrace.

    .. code:: bash

        zfs-replicate --verbose -l alunduil -i mypy.ini example.com bogus bogus.
    """
    runner = CliRunner()
    result = runner.invoke(
        main,
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
