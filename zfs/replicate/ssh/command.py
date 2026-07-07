"""SSH Command Generator."""

from typing import List

from ..command import Command
from .cipher import Cipher


def command(cipher: Cipher, user: str, key_file: str, port: int, host: str) -> Command:
    """Generate ssh commandline invocation."""
    options: List[str] = []

    if cipher == Cipher.FAST:
        options.extend(
            [
                "-c",
                "arcfour256,arcfour128,blowfish-cbc,aes128-ctr,aes192-ctr,aes256-ctr",
            ]
        )
    elif cipher == Cipher.DISABLED:
        options.extend(["-o", "noneenabled=yes", "-o", "noneswitch=yes"])

    options.extend(
        [
            "-i",
            key_file,
            "-o",
            "BatchMode=yes",
            "-o",
            "StrictHostKeyChecking=yes",
            "-o",
            "ConnectTimeout=7",
        ]
    )

    if user:
        options.extend(["-l", user])

    options.extend(["-p", str(port), host])

    return Command.with_empty_env("ssh", *options)
