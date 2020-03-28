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
    "--follow-delete", is_flag=True, help="Delete snapshots on REMOTE_FS that have been deleted from LOCAL_FS."
)
@click.option("--recursive", is_flag=True, help="Recursively replicate snapshots.")
@click.option(
    "--compression",
    type=EnumChoice(Compression),
    default=Compression.LZ4,
    help="One of: off (no compression), lz4 (fastest), pigz (all rounder), or plzip (best compression).",
)
def main(  # pylint: disable=too-many-arguments,too-many-locals
    dry_run: bool,
    follow_delete: bool,
    recursive: bool,
    compression: Compression,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""

    filesystem.create(r_filesystem, ssh_command=ssh_command)

    if verbose:
        click.echo(f"checking filesystem {host}/{r_filesystem.name}")

    r_snaps = snapshot.list(r_filesystem, recursive=recursive, ssh_command=ssh_command)

    if verbose:
        click.echo(f"found {len(r_snaps)} snapshots on {r_filesystem.name}")
        click.echo()

    filesystem_l_snaps = {
        filesystem: list(l_snaps) for filesystem, l_snaps in itertools.groupby(l_snaps, key=lambda x: x.filesystem)
    }
    filesystem_r_snaps = {
        filesystem: list(r_snaps) for filesystem, r_snaps in itertools.groupby(r_snaps, key=lambda x: x.filesystem)
    }
    tasks = task.generate(remote_fs, filesystem_l_snaps, filesystem_r_snaps, follow_delete=follow_delete)

    if verbose:
        click.echo(task.report(tasks))

    if not dry_run:
        filesystem_tasks = [
            (filesystem, list(tasks)) for filesystem, tasks in itertools.groupby(tasks, key=lambda x: x.filesystem)
        ]
        task.execute(
            remote_fs, filesystem_tasks, follow_delete=follow_delete, compression=compression, ssh_command=ssh_command
        )
