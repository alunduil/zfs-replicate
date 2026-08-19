# Replicate an encrypted data set

zfs-replicate replicates an encrypted data set without decrypting it, provided
you already replicate plain data sets and the source data set carries
encryption. Creating encrypted data sets and managing their keys are out of
scope. [`zfs-send(8)`] and [`zfs-recv(8)`] describe what raw sending and
receiving do.

## Replicate with the default raw send

An encrypted data set needs no extra send flag, because zfs-replicate passes
`-w` to `zfs send` by default:

```bash
zfs-replicate -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

## Point the replica at its key

A raw receive leaves the replica's `keylocation` at `prompt`. To read the key
from a file on the destination instead, set the property during the receive:

```bash
zfs-replicate --receive-set keylocation=file:///etc/zfs/keys/secrets.key \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

## Send decrypted data instead

Pass `--send-no-raw` when the destination needs readable data, or when its pool
lacks the `large_blocks` or `embedded_data` features. The second case catches
data sets with no encryption at all, because `-w` implies `-Lec`:

```bash
zfs-replicate --send-no-raw -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

Keep the same choice for every replication of a given destination data set. ZFS
refuses to mix raw and non-raw receives, so switching means replicating that
destination from scratch.

[`zfs-recv(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-recv.8.html
[`zfs-send(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-send.8.html
