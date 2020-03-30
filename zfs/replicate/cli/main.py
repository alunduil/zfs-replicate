"""Main function zfs-replicate."""

import itertools

import click

from .. import filesystem, snapshot, ssh, task
from ..compress import Compression
from ..filesystem import FileSystem
from ..filesystem import filesystem as filesystem_t
from ..ssh import Cipher
from .click import EnumChoice


@click.command()
@click.option("--dry-run", is_flag=True, help="Generate replication tasks but do not execute them.")
@click.option(
    "--compression",
    type=EnumChoice(Compression),
    default=Compression.LZ4,
    help="One of: off (no compression), lz4 (fastest), pigz (all rounder), or plzip (best compression).",
)
def main(  # pylint: disable=too-many-arguments,too-many-locals
    dry_run: bool,
    compression: Compression,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""

    if verbose:
        click.echo(task.report(tasks))

    if not dry_run:
        filesystem_tasks = [
            (filesystem, list(tasks)) for filesystem, tasks in itertools.groupby(tasks, key=lambda x: x.filesystem)
        ]
        task.execute(
            remote_fs, filesystem_tasks, follow_delete=follow_delete, compression=compression, ssh_command=ssh_command
        )
