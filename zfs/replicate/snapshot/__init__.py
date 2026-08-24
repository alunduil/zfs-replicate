"""ZFS Snapshot Operations."""

from .destroy import destroy
from .list import list_snapshots as list
from .send import send
from .type import Snapshot

# The definition is ``list_snapshots`` so it does not shadow the builtin inside
# its own module.
__all__ = ("Snapshot", "destroy", "list", "send")
