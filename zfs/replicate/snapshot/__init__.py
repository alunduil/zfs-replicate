"""ZFS Snapshot Operations."""

from .destroy import destroy as destroy  # pylint: disable=useless-import-alias
from .list import list as list  # pylint: disable=redefined-builtin,useless-import-alias
from .send import send as send  # pylint: disable=useless-import-alias
from .type import Snapshot as Snapshot  # pylint: disable=useless-import-alias
