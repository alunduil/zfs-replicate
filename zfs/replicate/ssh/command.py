"""SSH Command Generator."""

from ..command import Command
from .cipher import Cipher

_OPTIONS: dict[str, str | int] = {
    "BatchMode": "yes",
    "StrictHostKeyChecking": "yes",
    "ConnectTimeout": 7,
    # A peer that goes silent without closing the socket fails the transfer
    # after about a minute instead of hanging it.
    "ServerAliveInterval": 15,
    "ServerAliveCountMax": 4,
}


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

    for name, value in _OPTIONS.items():
        options.extend(_option(name, value))

    options.extend(["-i", key_file])

    if user:
        options.extend(["-l", user])

    options.extend(["-p", str(port), host])

    return Command.with_empty_env("ssh", *options)


def _option(name: str, value: str | int) -> list[str]:
    return ["-o", f"{name}={value}"]
