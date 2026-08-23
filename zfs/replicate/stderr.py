r"""Cleanup for stderr captured from a zfs command.

Two kinds of noise ride along with a command's stderr and do not belong in a
raised error message: the line endings the shell appends, and the none-cipher
banner an ssh build that supports it prints when
:data:`~zfs.replicate.ssh.Cipher.DISABLED` turns encryption off. Every call site
that reports stderr routes it through :func:`clean` first.
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
