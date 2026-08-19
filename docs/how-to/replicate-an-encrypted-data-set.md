# Replicate an encrypted data set

zfs-replicate replicates an encrypted data set without decrypting it, provided
you already replicate plain data sets and the source data set carries
encryption. Creating encrypted data sets and managing their keys are out of
scope. For why raw sends work this way and what they cost, see
[Raw sends](../explanation/raw-sends.md).

## Replicate with the default raw send

An encrypted data set needs no extra send flag, because zfs-replicate passes
`-w` to `zfs send` by default:

```bash
zfs-replicate -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

## Point the replica at its key

A raw receive leaves the replica's `keylocation` at `prompt` (see
[`zfs-recv(8)`]). To read the key from a file on the destination instead, set
the property during the receive:

```bash
zfs-replicate --receive-set keylocation=file:///etc/zfs/keys/secrets.key \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

## Send decrypted data instead

Pass `--send-no-raw` when the destination needs readable data, or when its pool
lacks the `large_blocks` or `embedded_data` features:

```bash
zfs-replicate --send-no-raw -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

Keep the same choice for every replication of a given destination data set.
Switching modes breaks later incremental sends.

[`zfs-recv(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-recv.8.html
