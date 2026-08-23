"""Cleanup for stderr captured from a zfs command.

Captured stderr carries noise that does not belong in a raised error message:
the line ending the shell appends, and the banner ssh prints when
:attr:`~zfs.replicate.ssh.Cipher.DISABLED` turns encryption off. Call sites
route stderr through :func:`clean` before reporting it.
"""

_NONE_CIPHER_WARNING = b"WARNING: ENABLED NONE CIPHER"


def clean(stderr: bytes) -> bytes:
    r"""Drop line-ending and none-cipher noise from captured stderr.

    >>> clean(b"cannot open 'pool/data': dataset does not exist\n")
    b"cannot open 'pool/data': dataset does not exist"

    >>> clean(b"WARNING: ENABLED NONE CIPHER\r\n")
    b''
    """
    return stderr.strip(b"\n").strip(b"\r").replace(_NONE_CIPHER_WARNING, b"")
