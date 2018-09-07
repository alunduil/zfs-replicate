Description
-----------

ZFS_ Replicate Utility

A small command line utility to handle remote replication of ZFS_ snapshots
using SSH_.

This project is directly based on the autorepl.py_ script used by FreeNAS_.

Getting Started
---------------

Usage is pretty straight-forward and documented in the command's help output.
For more information on ZFS_ snapshots, see:
`Working With Oracle Solaris ZFS Snapshots and Clones <https://docs.oracle.com/cd/E26505_01/html/E37384/gavvx.html#scrolltoc>`.

Remote Configuration
--------------------

If you're replicating using the root user's credentials, you should really
reconsider, but this script should just work.

Otherwise, you'll not only need to ensure the user you're using has SSH access
to the remote host, but also can mount filesystems (if this is desirable) and
has ZFS permissions configured correctly.

Allow user to mount (FreeBSD):

```
sysctl -w vfs.usermount=1
```

ZFS Permissions:

```
zfs allow backup create,destroy,snapshot,rollback,clone,promote,rename,mount,send,receive,quota,reservation,hold storage
```

More information about this configuration can be found at the following sources:

* `ZFS REMOTE REPLICATION SCRIPT WITH REPORTING <https://techblog.jeppson.org/2014/10/zfs-remote-replication-script-with-reporting/>`
* `ZFS replication without using Root user <https://forums.freenas.org/index.php?threads/zfs-replication-without-using-root-user.21731/>`

Compared to Other Tools
-----------------------

This tool is only for replication of snapshots.  It assumes that another system
is creating them on a regular basis.  It also doesn't require installation of
any special tools (other than the standard shell scripts) on the remote host.

Other tools fill other niches:

`sanoid <https://github.com/jimsalterjrs/sanoid>`
  A full snapshot management system.  It's companion application, syncoid,
  handles replication with many available options.

`zfs-replicate (BASH) <https://github.com/leprechau/zfs-replicate>`
  A very similar project.  The major differences are configuration style (our
  project uses parameters whereas this project uses a BASH script), and the
  system expectations (e.g., logging controls).

`znapzend <http://www.znapzend.org/>`
  Another scheduling and replicating system.

`zrep <http://www.bolthole.com/solaris/zrep/>`
  A SH script with several control commands for snapshot replication.

More information has been captured in `this survey <https://www.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools://www.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools/>`.

Reporting Issues
----------------

Any isseus discovered should be recorded on
`Github <https://github.com/alunduil/zfs-replicate>`_.  If you believe you've
found an error or have a suggestion for a new feature, please ensure that it is
reported.

If you would like to contribute a fix or new feature, please submit a pull
request.  This project follows
`git flow <http://nvie.com/posts/a-successful-git-branching-model/>`_ and
utilizes travis_ to automatically check pull requests before a manual review.

.. _autorepl.py: https://github.com/freenas/freenas/blob/master/gui/tools/autorepl.py
.. _FreeNAS: http://www.freenas.org/
.. _SSH: https://www.openssh.com/
.. _travis: https://travis-ci.org/aunduil/zfs-replicate
.. _ZFS: http://open-zfs.org/wiki/System_Administration
