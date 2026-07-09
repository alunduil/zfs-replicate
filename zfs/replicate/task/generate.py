"""Replication Tasks."""

from typing import Dict, List

from ..filesystem import FileSystem, remote_filesystem
from ..filesystem import filesystem as filesystem_t
from ..list import venn
from ..snapshot import Snapshot
from .type import Action, Task


def generate(
    remote: FileSystem,
    local_snapshots: Dict[FileSystem, List[Snapshot]],
    remote_snapshots: Dict[FileSystem, List[Snapshot]],
    follow_delete: bool = False,
) -> List[Task]:
    """Generate Tasks for replicating local snapshots to remote snapshots."""
    tasks = []

    for filesystem, local_snaps in local_snapshots.items():
        remote_snapshots = {
            filesystem_t(
                name=key.name.replace(remote.name + "/", ""),
                readonly=filesystem.readonly,
            ): value
            for key, value in remote_snapshots.items()
        }

        if filesystem not in remote_snapshots:
            tasks.append(
                Task(
                    action=Action.CREATE,
                    filesystem=remote_filesystem(remote, filesystem),
                    snapshot=None,
                )
            )
            tasks.extend([Task(action=Action.SEND, filesystem=remote, snapshot=s) for s in local_snaps])
            continue

        lefts: List[Snapshot]
        middles: List[Snapshot]
        rights: List[Snapshot]

        lefts, middles, rights = venn(local_snaps, remote_snapshots[filesystem])

        if not middles:
            tasks.extend(
                [
                    Task(
                        action=Action.DESTROY,
                        filesystem=remote_filesystem(remote, filesystem),
                        snapshot=s,
                    )
                    for s in rights
                ],
            )

        tasks.extend([Task(action=Action.SEND, filesystem=remote, snapshot=s) for s in lefts])

        if middles and follow_delete:
            tasks.extend(
                [
                    Task(
                        action=Action.DESTROY,
                        filesystem=remote_filesystem(remote, filesystem),
                        snapshot=s,
                    )
                    for s in rights
                ],
            )

    for remote_fs in remote_snapshots:
        filesystem = filesystem_t(
            name=remote_fs.name.replace(remote.name + "/", ""),
            readonly=remote_fs.readonly,
        )

        if filesystem not in local_snapshots:
            tasks.extend(
                [
                    Task(
                        action=Action.DESTROY,
                        filesystem=remote_filesystem(remote, filesystem),
                        snapshot=s,
                    )
                    for s in remote_snapshots[filesystem]
                ],
            )
            tasks.append(
                Task(
                    action=Action.DESTROY,
                    filesystem=remote_filesystem(remote, filesystem),
                    snapshot=None,
                )
            )

    return tasks
