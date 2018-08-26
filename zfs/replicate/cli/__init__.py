"""CLI for ZFS Snapshot Replicator."""

import click

from ..dataset import DataSet
from .. import command
from .. import dataset
from . import option
from .. import snapshot
from .. import ssh
from .. import task

@click.command()
@click.option(
    "--verbose", "-v",
    is_flag=True,
    help="Print additional output.",
    )
@click.option(
    "--dry-run",
    is_flag=True,
    help="Generate replication tasks but do not execute them.",
    )
@click.option(
    "--follow-delete",
    is_flag=True,
    help="Delete snapshots on REMOTE_FS that have been deleted from LOCAL_FS",
    )
@click.option(
    "--recursive",
    is_flag=True,
    help="Recursively replicate snapshots."
    )
@click.option(
    "--port", "-p",
    type=click.IntRange(1, 65535),
    default=22,
    help="Connect to SSH on PORT.",
    )
@click.option(
    "--login", "-l", "--user", "-u",
    metavar="USER",
    help="Connect to SSH as USER.",
    )
@click.option(
    "-i", "--identity-file",
    type=click.Path(exists=True, dir_okay=False),
    required=True,
    help="SSH identity file to use.",
    )
@click.option(
    "--cipher",
    type=option.Cipher,
    default=option.Cipher.STANDARD,
    help="""\
disabled = no ciphers
fast     = only fast ciphers
standard = default ciphers
""",
    )
@click.option(
    "--compression",
    type=option.Compression,
    default=option.Compression.LZ4,
    help="""\
off   = no compression
lz4   = fastest
pigz  = all rounder
plzip = best compression
""",
    )
@click.argument(
    "host",
    required=True,
    help="Replicate snapshots to HOST.",
    )
@click.argument(
    "remote",
    required=True,
    metavar="REMOTE_DATASET",
    help="Send snapshots to REMOTE_DATASET on HOST.",
    )
@click.argument(
    "local",
    required=True,
    metavar="LOCAL_DATASET",
    help="Send snapshots of LOCAL_DATASET to HOST.",
    )
def main(
        verbose: bool(),
        dry_run: bool(),
        follow_delete: bool(),
        recursive: bool(),
        port: int(),
        login: str(),
        identity_file: str(),
        cipher: option.Cipher,
        compression: option.Compression,
        host: str(),
        remote: DataSet,
        local: DataSet,
    ): # pylint: disable=too-many-arguments,too-many-locals
    """Main entry point into zfs-replicate."""

    ssh_command = ssh.command(cipher, login, identity_file, port, host)

    if verbose:
        click.echo("checking dataset {}".format(local))

    l_snaps = snapshot.list(local, recursive=recursive)
    # TODO Exclusions from snapshots to replicate.

    if verbose:
        click.echo("found {} local snapshots".format(len(l_snaps)))

    r_dataset = dataset.remote_name(remote, local)
    dataset.create(r_dataset, ssh_command=ssh_command)
    # TODO integrate into previous dataset.remote_readonly(remote)

    # TODO figure out truenas stuff
    # if not readonly_remote_dataset(r_dataset):
    #    pass

    if verbose:
        click.echo("checking snapshots on {}".format(host))

    # TODO Localize dataset names.
    r_snaps = snapshot.list(remote, recursive=recursive, ssh_command=ssh_command)

    if verbose:
        click.echo("found {} remote snapshots".format(len(r_snaps)))

    tasks = task.generate(l_snaps, r_snaps, follow_delete=follow_delete)

    if verbose:
        click.echo(task.report(tasks))

    if not dry_run:
        task.execute(
            tasks,
            follow_delete=follow_delete,
            compression=compression,
            ssh_command=ssh_command
            )
