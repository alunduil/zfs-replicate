"""SSH Command Generator."""

from ..command import Command
from .cipher import Cipher

# A peer that stops answering without closing the socket would otherwise block
# communicate() forever. ssh drops it once _SERVER_ALIVE_COUNT_MAX probes sent
# _SERVER_ALIVE_INTERVAL seconds apart go unanswered: about a minute. Setting
# the count overrides ssh_config so that window holds.
_SERVER_ALIVE_INTERVAL = 15
_SERVER_ALIVE_COUNT_MAX = 4


def command(cipher: Cipher, user: str, key_file: str, port: int, host: str) -> Command:
    """Generate ssh commandline invocation."""
    options: list[str] = []

    if cipher == Cipher.FAST:
        options.extend(
            [
                "-c",
                "arcfour256,arcfour128,blowfish-cbc,aes128-ctr,aes192-ctr,aes256-ctr",
            ]
        )
    elif cipher == Cipher.DISABLED:
        options.extend([*_option("noneenabled", "yes"), *_option("noneswitch", "yes")])

    options.extend(
        [
            *_option("BatchMode", "yes"),
            *_option("StrictHostKeyChecking", "yes"),
            *_option("ConnectTimeout", 7),
            *_option("ServerAliveInterval", _SERVER_ALIVE_INTERVAL),
            *_option("ServerAliveCountMax", _SERVER_ALIVE_COUNT_MAX),
        ]
    )

    options.extend(["-i", key_file])

    if user:
        options.extend(["-l", user])

    options.extend(["-p", str(port), host])

    return Command.with_empty_env("ssh", *options)


def _option(name: str, value: str | int) -> list[str]:
    """Spell ``name=value`` as an ``-o`` override."""
    return ["-o", f"{name}={value}"]
