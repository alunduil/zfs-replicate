"""SSH Command Generator."""

from .cipher import Cipher


def command(cipher: Cipher, user: str(), key: str(), port: int(), host: str()) -> str():
    """Generated ssh commandline invocation."""

    ssh = "/usr/bin/env ssh"

    options = ""

    if cipher == Cipher.FAST:
        options += " -c arcfour256,arcfour128,blowfish-cbc,aes128-ctr,aes192-ctr,aes256-ctr"
    elif cipher == Cipher.DISABLED:
        options += " -o noneenabled=yes" " -o noneswitch=yes"

    options += (" -i {}" " -o BatchMode=yes" " -o StrictHostKeyChecking=yes" " -o ConnectTimeout=7").format(key)

    if user:
        options += " -l {}".format(user)

    options += " -p {} {}".format(port, host)

    return ssh + options
