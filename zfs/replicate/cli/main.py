"""Main function zfs-replicate."""

import itertools

import click

from .. import filesystem, receive, send, snapshot, ssh, task
from ..compress import Compression
from ..filesystem import FileSystem
from ..filesystem import filesystem as filesystem_t
from ..ssh import Cipher
from . import log, options
from .click import EnumChoice

log.configure()


@click.command()  # type: ignore[misc]
@log.option  # type: ignore[misc]
@click.option(  # type: ignore[misc]
    "--dry-run",
    is_flag=True,
    help="Generate replication tasks but do not execute them.",
)
@click.option(  # type: ignore[misc]
    "--follow-delete",
    is_flag=True,
    help="Delete snapshots on REMOTE_FS that have been deleted from LOCAL_FS.",
)
@click.option("--recursive", is_flag=True, help="Recursively replicate snapshots.")  # type: ignore[misc]
@click.option(  # type: ignore[misc]
    "--port",
    "-p",
    type=click.IntRange(1, 65535),
    metavar="PORT",
    default=22,
    help="Connect to SSH on PORT.",
)
@click.option(  # type: ignore[misc]
    "--login",
    "-l",
    "--user",
    "-u",
    "user",
    metavar="USER",
    help="Connect to SSH as USER.",
)
@click.option(  # type: ignore[misc]
    "-i",
    "--identity-file",
    type=click.Path(exists=True, dir_okay=False),
    required=True,
    help="SSH identity file to use.",
)
@click.option(  # type: ignore[misc]
    "--cipher",
    type=EnumChoice(Cipher),
    default=Cipher.STANDARD,
    help="One of: disable (no ciphers), fast (only fast ciphers), or standard (default ciphers).",
)
@click.option(  # type: ignore[misc]
    "--compression",
    type=EnumChoice(Compression),
    default=Compression.LZ4,
    help="One of: off (no compression), lz4 (fastest), pigz (all rounder), or plzip (best compression).",
)
@options.send_group
@options.receive_group
@click.argument("host", required=True)  # type: ignore[misc]
@click.argument("remote_fs", type=filesystem_t, required=True, metavar="REMOTE_FS")  # type: ignore[misc]
@click.argument("local_fs", type=filesystem_t, required=True, metavar="LOCAL_FS")  # type: ignore[misc]
def main(  # pylint: disable=R0917,R0914,R0913
    dry_run: bool,
    follow_delete: bool,
    recursive: bool,
    port: int,
    user: str,
    identity_file: str,
    cipher: Cipher,
    compression: Compression,
    send_options: send.Options,
    receive_options: receive.Options,
    host: str,
    remote_fs: FileSystem,
    local_fs: FileSystem,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""
    ssh_command = ssh.command(cipher, user, identity_file, port, host)

    log.logger.info("checking filesystem %s", local_fs.name)

    l_snaps = snapshot.list(local_fs, recursive=recursive)
    # Improvement: exclusions from snapshots to replicate.

    log.logger.info("found %d snapshots on %s", len(l_snaps), local_fs.name)

    r_filesystem = filesystem.remote_dataset(remote_fs, local_fs)
    filesystem.create(r_filesystem, ssh_command=ssh_command)

    log.logger.info("checking filesystem %s/%s", host, r_filesystem.name)

    r_snaps = snapshot.list(r_filesystem, recursive=recursive, ssh_command=ssh_command)

    log.logger.info("found %d snapshots on %s", len(r_snaps), r_filesystem.name)

    filesystem_l_snaps = {
        filesystem: list(l_snaps)
        for filesystem, l_snaps in itertools.groupby(
            l_snaps, key=lambda x: x.filesystem
        )
    }
    filesystem_r_snaps = {
        filesystem: list(r_snaps)
        for filesystem, r_snaps in itertools.groupby(
            r_snaps, key=lambda x: x.filesystem
        )
    }
    tasks = task.generate(
        remote_fs, filesystem_l_snaps, filesystem_r_snaps, follow_delete=follow_delete
    )

    # The plan is the point of --dry-run, so print it to stdout on the user's
    # explicit request. Operational progress is on the logger instead.
    if dry_run:
        click.echo(task.report(tasks))

    if not dry_run:
        filesystem_tasks = [
            (filesystem, list(tasks))
            for filesystem, tasks in itertools.groupby(
                tasks, key=lambda x: x.filesystem
            )
        ]
        task.execute(
            remote_fs,
            filesystem_tasks,
            compression=compression,
            send_options=send_options,
            receive_options=receive_options,
            ssh_command=ssh_command,
        )
