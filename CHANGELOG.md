# Revision history for zfs-replicate

## unreleased

## 4.1.0 -- 2026-05-08

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
