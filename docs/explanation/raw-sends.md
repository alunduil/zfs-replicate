# Raw sends

`zfs send -w` sends blocks as they sit on disk. For an encrypted data set the
stream stays encrypted the whole way. The destination stores what the source
stored, and the host running the receive holds no key to read it. The source
doesn't need its keys loaded either, so replication works on a data set nobody
has unlocked. Without `-w`, `zfs send` decrypts on the way out, and protecting
the copy becomes the destination's problem rather than the source's. The
commands are in [Replicate an encrypted data
set](../how-to/replicate-an-encrypted-data-set.md).

That property is why zfs-replicate defaults to `-w`. The tool's usual job is
pushing snapshots to a backup host, where the weaker assumption is worth making.
The host holds the data, the same people might not administer it, and it has no
reason to need the readable copy. A raw send makes the destination's
trustworthiness stop mattering for confidentiality.

## What raw costs

A raw stream arrives as is. `zfs receive` can't decrypt it, re-encrypt it under
a key of the destination's own, or recompress it. Any policy the destination
wants to impose on the way in has to arrive some other way. The replica also
carries the same encryption keys as its source. That makes a raw send a poor fit
when the point of the copy is to hand it to someone who shouldn't hold those
keys.

The flag also does more than its name suggests on a data set with no encryption,
where it behaves the same as `-L -e -c`. A destination pool lacking the
`large_blocks` or `embedded_data` features can't receive such a stream.
zfs-replicate's default can therefore fail on an old pool that holds nothing
encrypted at all. That's the one case where the default is arguably wrong, and
`--send-no-raw` exists partly to escape it.

## Raw and non-raw don't mix

ZFS tracks the initialization vector set of a received data set, and the two
receive modes disagree about it. ZFS refuses a raw incremental receive that
follows a non-raw one. It permits a non-raw receive on top of a raw-received
data set, but that replaces the initialization vector set. Every later raw
incremental receive then fails with `IV set guid mismatch`.

Each destination data set therefore takes its mode at the first replication, and
changing it later means starting that destination over. Deciding at the outset
costs nothing. Discovering the constraint on an incremental costs a full resend.
