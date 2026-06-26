"""Main function zfs-replicate."""

import functools
import itertools
from typing import Callable, Dict, Tuple

import click

from .. import filesystem, snapshot, ssh, task
from ..compress import Compression
from ..filesystem import FileSystem
from ..filesystem import filesystem as filesystem_t
from ..snapshot import ReceiveOptions
from ..ssh import Cipher
from .click import EnumChoice


def receive_option_group(command: Callable[..., None]) -> Callable[..., None]:
    """Attach the ``--receive-*`` option group and collapse it to ReceiveOptions.

    Keeps the receive-side flags defined together and namespaced under
    ``--receive-`` so they can't collide with the global and transport
    options, and hands the command a single ``receive_options`` argument
    rather than one parameter per flag.
    """
    command = click.option(
        "--receive-set",
        "receive_set",
        multiple=True,
        metavar="KEY=VALUE",
        help=(
            "Set a property on the received data set (maps to -o KEY=VALUE on"
            " zfs receive). Repeatable, for example --receive-set readonly=on"
            " --receive-set canmount=noauto."
        ),
    )(command)
    command = click.option(
        "--receive-resume-token-capable",
        "receive_resume",
        is_flag=True,
        help=(
            "Pass -s to zfs receive so an interrupted transfer saves a resume"
            " token on the destination."
        ),
    )(command)
    command = click.option(
        "--receive-mount/--receive-no-mount",
        "receive_mount",
        default=True,
        help=(
            "Mount received data sets (default). Use --receive-no-mount to pass"
            " -u so they are left unmounted."
        ),
    )(command)
    command = click.option(
        "--receive-force/--receive-no-force",
        "receive_force",
        default=True,
        help=(
            "Pass -F to zfs receive so the destination rolls back to match the"
            " stream (default). Use --receive-no-force to fail instead of"
            " discarding divergent destination snapshots."
        ),
    )(command)

    @functools.wraps(command)
    def wrapper(  # pylint: disable=R0913
        *args: object,
        receive_force: bool,
        receive_mount: bool,
        receive_resume: bool,
        receive_set: Tuple[str, ...],
        **kwargs: object,
    ) -> None:
        return command(
            *args,
            receive_options=ReceiveOptions(
                force=receive_force,
                no_mount=not receive_mount,
                resume=receive_resume,
                properties=_parse_properties(receive_set),
            ),
            **kwargs,
        )

    return wrapper


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
@receive_option_group
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
    receive_options: ReceiveOptions,
    host: str,
    remote_fs: FileSystem,
    local_fs: FileSystem,
) -> None:
    """Replicate LOCAL_FS to REMOTE_FS on HOST."""
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
                f"expected KEY=VALUE, got {item!r}", param_hint="--receive-set"
            )
        parsed[key] = value

    return parsed
