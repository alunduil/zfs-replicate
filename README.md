# zfs-replicate

https://github.com/alunduil/zfs-replicate

By Alex Brandt <alunduil@gmail.com>

## Description

zfs-replicate sends all ZFS snapshots to a remote host via SSH.  zfs-replicate
does **not** create ZFS snapshots.

zfs-replicate is based on [autorepl.py] used by [FreeNAS].

zfs-replicate is related to several other projects which fit other niches:

1. [sanoid]: A full snapshot management system.  Its companion application,
   syncoid, handles replication with many available options.
1. [zfs-replicate (BASH)]: A very similar project.  The major differences are
   configuration style (our project uses parameters whereas this project uses a
   BASH script), and the system expectations (e.g., logging controls).
1. [znapzend]: Another scheduling and replicating system.
1. [zrep]: A SH script with several control commands for snapshot replication.

## Terms of Use

You are free to copy, modify, and distribute zfs-replicate with attribution
under the terms of the BSD-2-Clause licence.  See the LICENSE for details.

## Prerequisites

1. A remote system with a ZFS filesystem and the zfs CLI tools
1. SSH access to that remote system
1. If you're not using the root user remotely:
   1. Ensure the user can mount filesystems
      1. [FreeBSD]: `sysctl -w vfs.usermount=1`
   1. Add ZFS permissions
      1. Command: `zfs allow ${USER} ${PERMISSIONS} ${BACKUP_DATASET}`
      1. Permissions
         1. clone
         1. create
         1. destroy
         1. hold
         1. mount
         1. promote
         1. quota
         1. readonly
         1. receive
         1. rename
         1. reservation
         1. rollback
         1. send
         1. snapshot
1. A local ZFS filesystem and zfs CLI tools

_N.B., don't use the root user to access your remote system._

## How to use zfs-replicate

1. `poetry install`
1. `poetry run -- zfs-replicate --help`

## Documentation

* `zfs-replicate --help`: Help for zfs-replicate.
* LICENSE: Licence file explaining usage of zfs-replicate.
* [Survey of ZFS Replication Tools][survey]: Overview of various ZFS replication
  tools and their uses.
* [Working With Oracle Solaris ZFS Snapshots and Clones]: Oracle's guide to
  working with ZFS snapshots.
* [ZFS REMOTE REPLICATION SCRIPT WITH REPORTING]
* [ZFS replication without using Root user]: How to configure ZFS replication
  for a non-root user.

## Getting Support

* [GitHub issues]: Report any problems or features requests to GitHub issues.

[autorepl.py]: https://github.com/freenas/freenas/blob/master/gui/tools/autorepl.py
[FreeBSD]: https://www.freebsd.org/
[FreeNAS]: http://www.freenas.org/
[GitHub issues]: https://github.com/alunduil/zfs-replicate/issues
[sanoid]: https://github.com/jimsalterjrs/sanoid
[survey]: https://www.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools/
[Working With Oracle Solaris ZFS Snapshots and Clones]: https://docs.oracle.com/cd/E26505_01/html/E37384/gavvx.html#scrolltoc
[ZFS REMOTE REPLICATION SCRIPT WITH REPORTING]: https://techblog.jeppson.org/2014/10/zfs-remote-replication-script-with-reporting/
[zfs-replicate (BASH)]: https://github.com/leprechau/zfs-replicate
[ZFS replication without using Root user]: https://forums.freenas.org/index.php?threads/zfs-replication-without-using-root-user.21731/
[znapzend]: http://www.znapzend.org/
[zrep]: http://www.bolthole.com/solaris/zrep/
