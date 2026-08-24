"""ZFS Snapshot Operations."""

from .destroy import destroy
from .list import list_snapshots as list
from .send import send
from .type import Snapshot

# ``list`` is the public spelling of ``list_snapshots``, which is named to avoid
# shadowing the builtin inside its own module. A re-export under a different name
# cannot use the redundant-alias form that marks the others public, so the whole
# surface is declared here instead.
__all__ = ("Snapshot", "destroy", "list", "send")
