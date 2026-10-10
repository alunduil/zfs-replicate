"""Task Hypothesis Strategies."""

from hypothesis.strategies import just, one_of

from zfs.replicate.filesystem.type import filesystem
from zfs.replicate.task.type import CreateFilesystem, DestroyFilesystem, DestroySnapshot, SendSnapshot
from zfs_test.replicate_test.snapshot_test.strategies import SNAPSHOTS

LOCAL = filesystem("tank/data")

# One filesystem, so execute()'s deepest-first sort leaves a drawn list in order.
TASKS = one_of(
    just(CreateFilesystem(filesystem=LOCAL)),
    just(DestroyFilesystem(filesystem=LOCAL)),
    SNAPSHOTS.map(lambda s: SendSnapshot(filesystem=LOCAL, snapshot=s)),
    SNAPSHOTS.map(lambda s: DestroySnapshot(filesystem=LOCAL, snapshot=s)),
)
