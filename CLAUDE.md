# Project guide for Claude Code

## Tool inventory

Reach for these before writing a `curl` call, a manual API
request, or a one-off `Bash` helper.

- **Dependencies:** `Poetry`, resolved from `pyproject.toml`.
- **Development shell:** `.devcontainer/`. No `Nix` expressions
  live here: `nixpkgs` packages `zfs-replicate` upstream, so a
  local derivation would be a second definition to keep in sync.
- **Tests:** `pytest` over `zfs_test/`. The conventions it
  follows, from the class-per-symbol layout and its fixtures to
  when a property test beats a fixed input, live in
  [docs/reference/testing.md](docs/reference/testing.md).
- **Lint, format, and types:** match `ruff` and `mypy` when
  writing code. `pre-commit run --all-files` is the canonical
  local check, and `.pre-commit-config.yaml` lists everything
  else it gates.
- **Entry point:** `zfs.replicate.cli.main:main`, installed as
  `zfs-replicate`.
- **Session setup:** a `SessionStart` hook already ran `poetry
  install` and `pre-commit install`. Machine-specific overrides
  belong in `.claude/settings.local.json`.

## Scope discipline

- Keep one concern per pull request. Revert incidental edits
  before requesting review.
- Confirm the scope doesn't overlap a linked or sibling issue.
  If it might, ask in the issue thread.
- Milestones don't gate work. `release-please` cuts releases
  from merged commits, so a later milestone is no reason to
  defer an issue. An unreleased prerequisite is, and belongs on
  a `blocked-by` edge.

## Commit and pull request conventions

Squash merge lands the pull request title as the commit
message, so the title is the string `release-please` parses for
the version bump. Both follow
[CONTRIBUTING.md](CONTRIBUTING.md#commit-messages).
