<!-- vale RedHat.Headings = NO -->
# zfs-replicate
<!-- vale RedHat.Headings = YES -->

<https://github.com/alunduil/zfs-replicate>

By Alex Brandt <alunduil@gmail.com>

## Description

zfs-replicate sends all Zettabyte File System (ZFS) snapshots to a remote host by SSH.  zfs-replicate
does **not** create ZFS snapshots.

zfs-replicate forks [autorepl.py] used by [`FreeNAS`].

zfs-replicate relates to several other projects, which fit other niches:

1. [sanoid]: A full snapshot management system. Its companion,
   `syncoid`, handles replication with copious options.
1. [zfs-replicate (BASH)]: A similar project. The major differences include
   configuration style and system expectations (for example, logging controls).
   zfs-replicate uses parameters whereas zfs-replicate (BASH) uses a BASH script.
1. [znapzend]: Another scheduling and replicating system.
1. [zrep]: A SH script with several control commands for snapshot replication.

## Terms of use

You are free to copy, change, and distribute zfs-replicate with attribution
under the terms of the `BSD-2-Clause` licence. See the [LICENSE] for details.

## Prerequisites

1. A remote system with a ZFS filesystem and the `zfs` command-line tools
1. If using lz4 compression, local and remote systems must have lz4 in their environments
1. SSH access to that remote system
1. If you're not using the root user remotely:
   1. Ensure the user can mount filesystems
      1. [FreeBSD]: `sysctl -w vfs.usermount=1`
   1. Add ZFS permissions
      1. Command: `zfs allow ${USER} ${PERMISSIONS} ${BACKUP_DATASET}`
      1. Permissions
         1. `clone`
         1. `create`
         1. `destroy`
         1. `hold`
         1. `mount`
         1. `promote`
         1. `quota`
         1. `readonly`
         1. `receive`
         1. `rename`
         1. `reservation`
         1. `rollback`
         1. `send`
         1. `snapshot`
1. A local ZFS filesystem and `zfs` command-line tools

_N.B., don't use the root user to access your remote system._

## How to use zfs-replicate

1. `poetry install`
1. `poetry run -- zfs-replicate --help`

Nix and NixOS users install the `zfs-replicate` package from [nixpkgs] rather
than building from this repository. NixOS also provides a
`services.zfs.autoReplication` module that runs replication as a system
service.

## Setting properties on the replica

To match your destination's policy without a post-receive patch-up, set ZFS
properties on the received data set with `--receive-set KEY=VALUE` (repeatable)
and control mounting with `--receive-no-mount`. For example, to set the replica
`readonly` and keep it from mounting on the destination:

```bash
zfs-replicate --receive-no-mount --receive-set readonly=on --receive-set canmount=noauto \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

See `zfs-replicate --help` for the full set of `--receive-` flags.

## Tuning the send stream

To replicate a large-block, already-compressed data set without re-reading or
recompressing it on the way out, tune the `zfs send` stream with the `--send-`
flags:

```bash
zfs-replicate --send-large-block --send-compressed \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

See `zfs-replicate --help` for the full set of `--send-` flags.

## Replicating encrypted data sets

`zfs send -w` sends blocks as they sit on disk, so an encrypted data set arrives
on the destination still encrypted and the backup host holds no key to read it.
The replica carries the same encryption keys as its source, and the source
doesn't need those keys loaded to run the replication. Without `-w`, `zfs send`
decrypts on the way out and the destination holds plain text.

zfs-replicate passes `-w` by default, so replicating an encrypted data set to a
less-trusted host takes no extra send flags. A raw receive does leave the
replica's `keylocation` at `prompt`, so pair the send with `--receive-set` to
point the destination at a key file:

```bash
zfs-replicate --receive-set keylocation=file:///etc/zfs/keys/secrets.key \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

Two situations call for `--send-no-raw` instead:

1. The destination needs readable data, either to mount the replica or to
   re-encrypt it under a key of its own. `zfs receive` takes a raw stream as is
   and can't decrypt, re-encrypt, or recompress it.
1. The destination pool lacks the `large_blocks` or `embedded_data` features.
   On a data set with no encryption, `-w` behaves the same as `-L -e -c`, so
   the default send needs both features.

Choose one mode per destination data set and stay with it. ZFS refuses a raw
incremental receive that follows a non-raw one. A non-raw receive on top of a
raw-received data set also replaces the initialization vector set, so every
later raw incremental receive fails with an `IV set guid mismatch` error.

## Documentation

* `zfs-replicate --help`: Help for zfs-replicate.
* [LICENSE]: Licence file explaining usage of zfs-replicate.
* [Survey of ZFS Replication Tools][survey]: Overview of various ZFS replication
  tools and their uses.
* [Working With Oracle Solaris ZFS Snapshots and Clones]: Oracle's guide to
  working with ZFS snapshots.
<!-- vale RedHat.Definitions = NO -->
* [ZFS REMOTE REPLICATION SCRIPT WITH REPORTING]
<!-- vale RedHat.Definitions = YES -->
* [ZFS replication without using Root user]: How to configure ZFS replication
  for a non-root user.

## Getting support

* [GitHub issues]: Report any problems or features requests to GitHub issues.

[autorepl.py]: https://github.com/freenas/freenas/blob/master/gui/tools/autorepl.py
[FreeBSD]: https://www.freebsd.org/
[`FreeNAS`]: http://www.freenas.org/
[GitHub issues]: https://github.com/alunduil/zfs-replicate/issues
[LICENSE]: ./LICENSE
[nixpkgs]: https://search.nixos.org/packages?show=zfs-replicate
[sanoid]: https://github.com/jimsalterjrs/sanoid
[survey]: https://www.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools/
[Working With Oracle Solaris ZFS Snapshots and Clones]: https://docs.oracle.com/cd/E26505_01/html/E37384/gavvx.html#scrolltoc
[ZFS REMOTE REPLICATION SCRIPT WITH REPORTING]: https://techblog.jeppson.org/2014/10/zfs-remote-replication-script-with-reporting/
[zfs-replicate (BASH)]: https://github.com/leprechau/zfs-replicate
[ZFS replication without using Root user]: https://forums.freenas.org/index.php?threads/zfs-replication-without-using-root-user.21731/
[znapzend]: http://www.znapzend.org/
[zrep]: http://www.bolthole.com/solaris/zrep/
