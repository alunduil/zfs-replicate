# How to replicate an encrypted data set

Replicate an encrypted data set to a remote host without decrypting it, and
leave the replica able to load its own key. This assumes you already replicate
plain data sets with zfs-replicate.

## Replicate the data set

Set the replica's key location during the receive, so that it doesn't fall back
to `prompt`:

```bash
zfs-replicate --receive-set keylocation=file:///etc/zfs/keys/secrets.key \
  -l backup -i ~/.ssh/id_ed25519 backup.example.com tank/backups tank/secrets
```

## Confirm the replica arrived encrypted

Check the replica on the destination. zfs-replicate nests it as
`REMOTE_FS/POOL/DATA_SET`, so `tank/secrets` lands at
`tank/backups/tank/secrets`:

```bash
ssh backup@backup.example.com \
  zfs get encryption,keylocation tank/backups/tank/secrets
```

## Load the key on the destination

```bash
ssh backup@backup.example.com zfs load-key tank/backups/tank/secrets
```

[`zfs-send(8)`] and [`zfs-recv(8)`] describe what raw sending and receiving do.

[`zfs-recv(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-recv.8.html
[`zfs-send(8)`]: https://openzfs.github.io/openzfs-docs/man/master/8/zfs-send.8.html
