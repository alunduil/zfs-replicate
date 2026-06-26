"""Main function zfs-replicate."""

import itertools
from typing import Dict, Tuple

import click

from .. import filesystem, snapshot, ssh, task
from ..compress import Compression
from ..filesystem import FileSystem
from ..filesystem import filesystem as filesystem_t
from ..ssh import Cipher
from .click import EnumChoice


@click.command()  # type: ignore[misc]
@click.option("--verbose", "-v", is_flag=True, help="Print additional output.")  # type: ignore[misc]
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
@click.option(  # type: ignore[misc]
    "--raw/--no-raw",
    default=True,
    help=(
        "Pass --raw to zfs send so encrypted datasets replicate without"
        " decryption (default). Use --no-raw to send decrypted data, for"
        " example when the destination cannot preserve encryption."
    ),
)
@click.option(  # type: ignore[misc]
    "--force/--no-force",
    default=True,
    help=(
        "Pass -F to zfs receive so the destination rolls back to match the"
        " stream (default). Use --no-force to fail instead of discarding"
        " divergent destination snapshots."
    ),
)
@click.option(  # type: ignore[misc]
    "--no-mount",
    is_flag=True,
    help="Pass -u to zfs receive so received datasets are not mounted.",
)
@click.option(  # type: ignore[misc]
    "--resume-token-capable",
    "-s",
    is_flag=True,
    help=(
        "Pass -s to zfs receive so an interrupted transfer saves a resume"
        " token on the destination."
    ),
)
@click.option(  # type: ignore[misc]
    "--set",
    "properties",
    multiple=True,
    metavar="KEY=VALUE",
    help=(
        "Set a property on the received dataset (maps to -o KEY=VALUE on zfs"
        " receive). Repeatable, for example --set readonly=on --set"
        " canmount=noauto."
    ),
)
@click.argument("host", required=True)  # type: ignore[misc]
@click.argument("remote_fs", type=filesystem_t, required=True, metavar="REMOTE_FS")  # type: ignore[misc]
@click.argument("local_fs", type=filesystem_t, required=True, metavar="LOCAL_FS")  # type: ignore[misc]
def main(  # pylint: disable=R0917,R0914,R0913
    verbose: bool,
    dry_run: bool,
    follow_delete: bool,
    recursive: bool,
    port: int,
    user: str,
    identity_file: str,
    cipher: Cipher,
    compression: Compression,
    raw: bool,
    force: bool,
    no_mount: bool,
    resume_token_capable: bool,
    properties: Tuple[str, ...],
    host: str,
    remote_fs: FileSystem,
    local_fs: FileSystem,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""
    receive_options = snapshot.ReceiveOptions(
        force=force,
        no_mount=no_mount,
        resume=resume_token_capable,
        properties=_parse_properties(properties),
    )

    ssh_command = ssh.command(cipher, user, identity_file, port, host)

    if verbose:
        click.echo(f"checking filesystem {local_fs.name}")

    l_snaps = snapshot.list(local_fs, recursive=recursive)
    # Improvement: exclusions from snapshots to replicate.

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

    if verbose:
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
            follow_delete=follow_delete,
            compression=compression,
            raw=raw,
            receive_options=receive_options,
            ssh_command=ssh_command,
        )


def _parse_properties(properties: Tuple[str, ...]) -> Dict[str, str]:
    parsed: Dict[str, str] = {}

    for item in properties:
        key, separator, value = item.partition("=")
        if not separator or not key:
            raise click.BadParameter(
                f"expected KEY=VALUE, got {item!r}", param_hint="--set"
            )
        parsed[key] = value

    return parsed
