"""Test structured command building and remote wrapping."""

import zfs.replicate.command as sut


def test_with_empty_env_prefixes_env() -> None:
    """The env-empty prefix goes in front of the program and its args."""
    assert sut.Command.with_empty_env("zfs", "list", "-H").argv == [
        "/usr/bin/env",
        "-",
        "zfs",
        "list",
        "-H",
    ]


def test_render_quotes_shell_metacharacters() -> None:
    """Quoting protects any argv token a remote shell would otherwise reparse."""
    rendered = sut.Command("zfs", ["destroy", "pool/a b; $x"]).render()

    assert rendered == "zfs destroy 'pool/a b; $x'"


def test_render_leaves_safe_tokens_unquoted() -> None:
    """Safe tokens with no shell-special characters are left unquoted."""
    assert sut.Command("zfs", ["list", "pool/data"]).render() == "zfs list pool/data"


def test_over_ssh_appends_single_quoted_argument() -> None:
    """Wrapping hands ssh the command as one shell-safe argument."""
    ssh = sut.Command.with_empty_env("ssh", "host")
    wrapped = sut.over_ssh(ssh, sut.Command.with_empty_env("zfs", "receive", "pool/a b"))

    assert wrapped.program == "/usr/bin/env"
    assert wrapped.args[:3] == ["-", "ssh", "host"]
    assert wrapped.args[-1] == "/usr/bin/env - zfs receive 'pool/a b'"


def test_over_ssh_joins_commands_as_a_pipeline() -> None:
    """Multiple wrapped commands become a single ' | ' remote pipeline argument."""
    ssh = sut.Command.with_empty_env("ssh", "host")
    wrapped = sut.over_ssh(
        ssh,
        sut.Command.with_empty_env("lz4", "-d"),
        sut.Command.with_empty_env("zfs", "receive"),
    )

    assert wrapped.args[-1] == "/usr/bin/env - lz4 -d | /usr/bin/env - zfs receive"
