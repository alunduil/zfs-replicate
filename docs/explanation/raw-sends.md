# Raw sends

zfs-replicate passes `-w` to `zfs send` by default. Its usual job is pushing
snapshots to a backup host, which holds the data without needing to read it and
which the same people might not administer. A raw send keeps the stream
encrypted the whole way, so that host stores a copy it has no key for.

Two consequences belong to the default rather than to the flag.

A data set with no encryption still gets `-Lec` behaviour, so a destination pool
without the `large_blocks` and `embedded_data` features can't receive a stream
that carries nothing encrypted at all.

The mode is permanent per destination data set. ZFS refuses to mix raw and
non-raw receives, so switching means replicating that destination from scratch.

[`zfs-send(8)`] and [`zfs-recv(8)`] describe what raw sending and receiving do,
including the exact error when the modes are mixed. The commands are in
[Replicate an encrypted data set](../how-to/replicate-an-encrypted-data-set.md).

[`zfs-recv(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-recv.8.html
[`zfs-send(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-send.8.html
