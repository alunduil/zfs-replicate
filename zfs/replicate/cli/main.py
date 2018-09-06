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
@click.option("--verbose", "-v", is_flag=True, help="Print additional output.")
@click.option("--dry-run", is_flag=True, help="Generate replication tasks but do not execute them.")
@click.option(
    "--follow-delete", is_flag=True, help="Delete snapshots on REMOTE_FS that have been deleted from LOCAL_FS."
)
@click.option("--recursive", is_flag=True, help="Recursively replicate snapshots.")
@click.option("--port", "-p", type=click.IntRange(1, 65535), metavar="PORT", default=22, help="Connect to SSH on PORT.")
@click.option("--login", "-l", "--user", "-u", metavar="USER", help="Connect to SSH as USER.")
@click.option(
    "-i",
    "--identity-file",
    type=click.Path(exists=True, dir_okay=False),
    required=True,
    help="SSH identity file to use.",
)
@click.option(
    "--cipher",
    type=EnumChoice(Cipher),
    default=Cipher.STANDARD,
    help="One of: disable (no ciphers), fast (only fast ciphers), or standard (default ciphers).",
)
@click.option(
    "--compression",
    type=EnumChoice(Compression),
    default=Compression.LZ4,
    help="One of: off (no compression), lz4 (fastest), pigz (all rounder), or plzip (best compression).",
)
@click.argument("host", required=True)
@click.argument("remote_fs", type=filesystem_t, required=True, metavar="REMOTE_FS")
@click.argument("local_fs", type=filesystem_t, required=True, metavar="LOCAL_FS")
def main(  # pylint: disable=too-many-arguments,too-many-locals
    verbose: bool,
    dry_run: bool,
    follow_delete: bool,
    recursive: bool,
    port: int,
    user: str,
    identity_file: str,
    cipher: Cipher,
    compression: Compression,
    host: str,
    remote_fs: FileSystem,
    local_fs: FileSystem,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""

    ssh_command = ssh.command(cipher, user, identity_file, port, host)

    if verbose:
        click.echo(f"checking filesystem {local_fs.name}")

    l_snaps = snapshot.list(local_fs, recursive=recursive)
    # Improvment: exclusions from snapshots to replicate.

    if verbose:
        click.echo(f"found {len(l_snaps)} snapshots on {local_fs.name}")
        click.echo()

    r_filesystem = filesystem.remote_dataset(remote_fs, local_fs)
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
