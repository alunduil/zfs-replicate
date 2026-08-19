# Replicate an encrypted data set

This guide replicates an encrypted data set to a remote host without decrypting
it on the way, and points the replica at its key on the destination. It assumes
you already replicate plain data sets with zfs-replicate and that the source
data set carries encryption. Creating encrypted data sets and managing
their keys are out of scope. For why raw sends behave this way and what they
cost, see [Raw sends](../explanation/raw-sends.md).

## Replicate with the default raw send

zfs-replicate passes `-w` to `zfs send` by default, so an encrypted data set
needs no extra send flag:

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
lacks the `large_blocks` or `embedded_data` features:

```bash
zfs-replicate --send-no-raw -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/data
```

Keep the same choice for every replication of a given destination data set.
Switching modes breaks later incremental sends, and recovering means replicating
the destination from scratch.
