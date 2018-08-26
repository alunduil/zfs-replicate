---
Description
---

ZFS_ Replicate Utility

A small command line utility to handle remote replication of ZFS_ snapshots
using SSH_.

This project is directly based on the autorepl.py_ script used by FreeNAS_.

---
Getting Started
---

Usage is pretty straight-forward and documented in the command's help output.
For more information on ZFS_ snapshots, see:
`Working With Oracle Solaris ZFS Snapshots and Clones <https://docs.oracle.com/cd/E26505_01/html/E37384/gavvx.html#scrolltoc>`.

---
Compared to Other Tools
---

TODO

---
Reporting Issues
---

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
