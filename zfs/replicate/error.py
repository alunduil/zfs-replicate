"""ZFS Replication Errors."""

from typing import Any

from click import ClickException


class ZFSReplicateError(ClickException):
    """Base ZFS Replication Error."""

    def __init__(self, message: str, *_args: Any) -> None:
        super().__init__(message)
