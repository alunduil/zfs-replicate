# Revision history for zfs-replicate

## unreleased

### Added

- Send-side flags for `zfs send`, grouped under the `--send-` prefix and mirroring `--receive-`: `--send-large-block` passes `-L`, `--send-embed` passes `-e`, `--send-compressed` passes `-c`, `--send-props` passes `-p`, and `--send-raw / --send-no-raw` toggles `-w` (default on, for encrypted data sets) (#392).
- Receive-side flags for `zfs receive`, grouped under the `--receive-` prefix: `--receive-force / --receive-no-force` toggles `-F` (default on), `--receive-mount / --receive-no-mount` toggles `-u` (default mounts), `--receive-resume-token-capable` passes `-s`, and the repeatable `--receive-set KEY=VALUE` maps to `-o KEY=VALUE` for properties such as `readonly`, `canmount`, or `mountpoint` on the replica without a post-receive patch-up (#393).

### Changed

- Renamed `--raw / --no-raw` (4.1.0) to `--send-raw / --send-no-raw` so every send flag shares the `--send-` prefix; the default still sends `zfs send -w` for encrypted data sets (#392).
- `--follow-delete` no longer implies `zfs send -p`. Send properties are now controlled solely by `--send-props`; pass it alongside `--follow-delete` to keep the earlier behaviour (#392).

## 4.1.0 -- 2026-05-09

### Added

- `--raw / --no-raw` toggles `zfs send --raw`. The default `--raw` preserves the existing behaviour for encrypted datasets; pass `--no-raw` when replicating to a destination that can't preserve encryption (#391).

## 4.0.1 -- 2026-05-08

### Fixed

- Snapshot equality no longer matches across unrelated data sets that share a name suffix (for example, `pool/data` and `bigpool/data`), preventing the task planner from acting on the wrong data set (#390).

## 4.0.0 -- 2024-11-24

### Added

### Changed

- Change invocations of lz4c to lz4

### Deprecated

### Fixed

### Removed

### Security
