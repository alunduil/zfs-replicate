"""ZFS Replication Errors."""

import logging
from typing import Any

from click import ClickException

logger = logging.getLogger(__name__)


class ZFSReplicateError(ClickException):  # type: ignore[misc]
    """Base ZFS Replication Error."""

    def __init__(self, message: str, *_args: Any) -> None:
        """Construct ZFSReplicateError."""
        # Log before propagating so the record lands even when click renders the
        # exception to stderr on its own.
        logger.error(message)
        super().__init__(message)
