<!-- vale RedHat.Headings = NO -->
# zfs-replicate
<!-- vale RedHat.Headings = YES -->

[![Licence](https://img.shields.io/github/license/alunduil/zfs-replicate)][LICENSE]
[![Coverage](https://img.shields.io/codecov/c/github/alunduil/zfs-replicate)][Codecov]
[![Python versions](https://img.shields.io/pypi/pyversions/zfs-replicate)][PyPI]

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

## Prerequisites

1. A local ZFS filesystem and `zfs` command-line tools
1. Python 3.10 or later on the local system
1. A remote system with a ZFS filesystem and the `zfs` command-line tools
1. SSH access to that remote system
1. `lz4` on both systems, unless you pass `--compression off`

Don't use the root user on the remote system. Delegate the ZFS permissions
replication needs on the backup data set to a regular user instead:

```sh
# FreeBSD only: let non-root users mount filesystems.
sysctl -w vfs.usermount=1

zfs allow "${USER}" clone,create,destroy,hold,mount,promote,quota,readonly,receive,rename,reservation,rollback,send,snapshot "${BACKUP_DATASET}"
```

## Install zfs-replicate

Install zfs-replicate from [PyPI]:

```sh
pipx install zfs-replicate
```

Nix and NixOS users install the `zfs-replicate` package from [nixpkgs]. NixOS
also provides a `services.zfs.autoReplication` module that runs replication as
a system service.

## How to use zfs-replicate

Replicate the snapshots of `LOCAL_FS` to `REMOTE_FS` on `HOST`:

```sh
zfs-replicate --user "${USER}" --identity-file ~/.ssh/id_ed25519 HOST REMOTE_FS LOCAL_FS
```

`zfs-replicate --help` lists every option. To tune the send stream or set
properties on the replica, see
[How to tune the send and receive streams][stream tuning].

## Documentation

* [How to replicate an encrypted data set][encrypted replication]: Replicate
  without decrypting, then load the replica's key on the destination.
* [How to tune the send and receive streams][stream tuning]: Control what the
  send stream carries and set properties on the replica.
* [CHANGELOG]: Changes in each release.
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

## Contributing

Contributions are welcome as issues or pull requests. [CONTRIBUTING] explains
how to set up a development environment and what a pull request needs.
Everyone taking part follows the [Code of Conduct].

## Licence

You are free to copy, change, and distribute zfs-replicate with attribution
under the terms of the `BSD-2-Clause` licence. See the [LICENSE] for details.

[autorepl.py]: https://github.com/truenas/middleware/blob/9cebb519bf4c6dc0c76b9ef00a82a9e82a54d724/gui/tools/autorepl.py
[CHANGELOG]: https://github.com/alunduil/zfs-replicate/blob/master/CHANGELOG.md
[Code of Conduct]: https://github.com/alunduil/zfs-replicate/blob/master/CODE_OF_CONDUCT.md
[Codecov]: https://app.codecov.io/gh/alunduil/zfs-replicate
[CONTRIBUTING]: https://github.com/alunduil/zfs-replicate/blob/master/CONTRIBUTING.md
[encrypted replication]: https://github.com/alunduil/zfs-replicate/blob/master/docs/how-to/replicate-an-encrypted-data-set.md
[`FreeNAS`]: https://www.truenas.com/
[GitHub issues]: https://github.com/alunduil/zfs-replicate/issues
[LICENSE]: https://github.com/alunduil/zfs-replicate/blob/master/LICENSE
[nixpkgs]: https://search.nixos.org/packages?show=zfs-replicate
[PyPI]: https://pypi.org/project/zfs-replicate/
[sanoid]: https://github.com/jimsalterjrs/sanoid
[stream tuning]: https://github.com/alunduil/zfs-replicate/blob/master/docs/how-to/tune-the-send-and-receive-streams.md
[survey]: https://www.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools/
[Working With Oracle Solaris ZFS Snapshots and Clones]: https://docs.oracle.com/cd/E26505_01/html/E37384/gavvx.html#scrolltoc
[ZFS REMOTE REPLICATION SCRIPT WITH REPORTING]: https://techblog.jeppson.org/2014/10/zfs-remote-replication-script-with-reporting/
[zfs-replicate (BASH)]: https://github.com/aaronhurt/zfs-replicate
[ZFS replication without using Root user]: https://www.truenas.com/community/threads/zfs-replication-without-using-root-user.21731/
[znapzend]: http://www.znapzend.org/
[zrep]: http://www.bolthole.com/solaris/zrep/
