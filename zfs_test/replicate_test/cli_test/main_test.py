from click.testing import CliRunner

from zfs.replicate.cli.main import main


def test_invokes_without_stacktrace() -> None:
    """zfs-replicate -l alunduil -i mypy.ini example.com bogus bogus => No stacktrace"""

    runner = CliRunner()
    result = runner.invoke(main, ["-l", "alunduil", "-i", "mypy.ini", "example.com", "bogus", "bogus"])
    assert isinstance(result.exception, SystemExit), "Expected normal exit."
