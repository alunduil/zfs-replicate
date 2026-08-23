# Project guide for Claude Code

## Tool inventory

Reach for these before writing a `curl` call, a manual API
request, or a one-off `Bash` helper.

- **Package manager:** `Poetry`. `pyproject.toml` is the source
  of truth, `poetry.lock` pins the resolved versions, and
  `poetry.toml` carries `Poetry` settings.
- **Development shell:** `.devcontainer/`, bootstrapped by
  `.devcontainer/post-create.sh`. No `Nix` expressions live
  here: `nixpkgs` packages `zfs-replicate` upstream, so a local
  derivation would be a second definition to keep in sync
  (#430).
- **Tests:** `pytest` over `zfs_test/`, configured under
  `[tool.pytest.ini_options]`. `--doctest-modules` runs every
  docstring example in `zfs/` as a test. `Hypothesis` is a
  development dependency.
- **Lint and format:** `ruff` (lint and format), `mypy`
  (types), `Vale` (prose), `vulture` (dead code), and
  `FawltyDeps` (unused dependencies), each gated through
  `pre-commit`. Run `pre-commit run --all-files` as the
  canonical local check.
- **Entry point:** the command-line tool installs as
  `zfs-replicate = "zfs.replicate.cli.main:main"`.

## Scope discipline

- Keep one concern per pull request. Revert incidental edits
  before requesting review.
- Confirm the scope doesn't overlap a linked or sibling issue.
  If it might, ask in the issue thread.
- Milestones don't gate work. `release-please` cuts releases
  from merged commits, so a later milestone is no reason to
  defer an issue. An unreleased prerequisite is, and belongs
  on a `blocked-by` edge.

## Commit and pull request conventions

Squash merge lands the pull request title as the commit
message, so the title is what `release-please` parses for the
version bump. Both follow the conventional-commit rules in
[CONTRIBUTING.md](CONTRIBUTING.md#commit-messages).

## Session defaults

A `SessionStart` hook in `.claude/settings.json` runs `poetry
install` and `pre-commit install`, so dependencies and git
hooks are ready before the first turn. Machine-specific
overrides belong in `.claude/settings.local.json`, which
`.gitignore` excludes.
