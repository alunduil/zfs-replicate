from click.testing import CliRunner, Result

from zfs.replicate.cli.main import main


def test_invokes_without_stacktrace() -> None:
    """zfs-replicate -l alunduil -i mypy.ini example.com bogus bogus => No stacktrace"""

    runner = CliRunner()
    result = runner.invoke(main, ["-l", "alunduil", "-i", "mypy.ini", "example.com", "bogus", "bogus"])
    assert isinstance(result.exception, SystemExit) or file_not_found_error(
        result
    ), "Expected SystemExit or FileNotFoundError."


def test_invokes_without_stacktrace_verbose() -> None:
    """zfs-replicate --verbose -l alunduil -i mypy.ini example.com bogus bogus => No stacktrace"""

    runner = CliRunner()
    result = runner.invoke(main, ["--verbose", "-l", "alunduil", "-i", "mypy.ini", "example.com", "bogus", "bogus"])
    assert isinstance(result.exception, SystemExit) or file_not_found_error(
        result
    ), "Expected SystemExit or FileNotFoundError."


def file_not_found_error(result: Result) -> bool:
    return bool(FileNotFoundError(2, "No such file or directory: '/usr/bin/env'") == result.exception)
