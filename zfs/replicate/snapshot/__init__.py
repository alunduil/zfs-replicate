"""ZFS Snapshot Operations."""
from .destroy import destroy as destroy  # noqa: F401 # pylint: disable=C0414
from .list import list as list  # noqa: F401 # pylint: disable=C0414,W0622
from .send import send as send  # noqa: F401 # pylint: disable=C0414
from .type import Snapshot as Snapshot  # noqa: F401 # pylint: disable=C0414
