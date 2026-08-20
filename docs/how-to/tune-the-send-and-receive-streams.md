# How to tune the send and receive streams

Change what `zfs send` puts on the wire, and what `zfs receive` does with the
stream on the destination. This assumes you already replicate a data set with
zfs-replicate.

Run `zfs-replicate --help` for the full set of `--send-` and `--receive-` flags.
[`zfs-send(8)`] and [`zfs-recv(8)`] describe what each underlying flag does.

## Send large, already-compressed blocks unchanged

Replicate a large-block, compressed data set without re-reading or
recompressing it on the way out:

```bash
zfs-replicate --send-large-block --send-compressed \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

`--send-large-block` requires the `large_blocks` feature on the destination
pool. [How to replicate an encrypted data set][encrypted replication] covers
`--send-raw`, which zfs-replicate passes by default.

## Set properties on the replica

Apply the destination's policy during the receive, so the replica never exists
in the wrong state. `--receive-set KEY=VALUE` repeats, and `--receive-no-mount`
keeps the replica from mounting:

```bash
zfs-replicate --receive-no-mount --receive-set readonly=on --receive-set canmount=noauto \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

zfs-replicate nests the replica as `REMOTE_FS/POOL/DATA_SET`, so `tank/data`
lands at `tank/backups/tank/data`. Check the properties there:

```bash
ssh backup@backup.example.com zfs get readonly,canmount tank/backups/tank/data
```

[encrypted replication]: ./replicate-an-encrypted-data-set.md
[`zfs-recv(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-recv.8.html
[`zfs-send(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-send.8.html
