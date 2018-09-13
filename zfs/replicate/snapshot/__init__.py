"""ZFS Snapshot Operations."""

from .destroy import destroy
from .list import list  # pylint: disable=redefined-builtin
from .send import send
from .type import Snapshot
