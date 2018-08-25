"""Shell Command Functions."""

from zfs.replicate.ssh import Cipher

from .ssh import ssh


def throttle_command(rate: int()) -> str():
    """Generated throttle commandline invocation."""

    throttle = ""

    if rate != 0:
        throttle = "/usr/bin/env throttle -K {} | ".format(rate)

    return throttle

def zfs_command(filesystem: str(), recursive: bool()) -> str():
    """Generated zfs commandline invocation."""

    zfs = "/usr/bin/env zfs"

    options = (
        " list"
        " -H"
        " -t snapshot"
        " -p"
        " -o name,creation"
        " -r"
        )

    if recursive:
        options += " -d 1"

    return zfs + options + " '{}'".format(filesystem)
