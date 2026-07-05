# CLAUDE.md for `zfs-replicate`

Repository-specific guidance for Claude Code sessions. The two sections below cover friction patterns that have shown up repeatedly while working in this codebase. Keep edits here calibrated to this repository; generic best practices belong elsewhere.

## 1. Tooling inventory

Discover existing tooling before reaching for `curl`, manual API calls, or first-principles scripts. The following is what this repository already ships:

- **Package manager:** `Poetry` (`pyproject.toml` plus `poetry.lock`, `poetry.toml`).
- **Development shell:** `Nix` (`shell.nix` imports `nix/shell.nix`); `.envrc` for `direnv`; `.devcontainer/` for Visual Studio Code.
- **Entry point:** `zfs-replicate = "zfs.replicate.cli.main:main"` (defined under `[tool.poetry.scripts]`).
- **Tests:** `pytest` with `--doctest-modules --cov=zfs --cov-report=term-missing` (configured in `pyproject.toml`); test tree under `zfs_test/`; `Hypothesis` is available as a development dependency.
- **Lint, format, and type-check:** `pre-commit` (`.pre-commit-config.yaml`) bundles `black`, `isort`, `flake8` (`.flake8`), `bandit`, `pylint` (`.pylintrc`), `pydocstyle`, `mypy` (`mypy.ini`, `strict = True`), and `vulture`. Prose linting uses `Vale` (`.vale.ini` plus `styles/`). Run `pre-commit run --all-files` before each commit rather than calling each tool individually.

## 2. Scope discipline

Use GitHub milestones for current release scope. The `blocked` label is in active use. Issues are sequenced; out-of-scope edits and early work on blocked items have both caused churn here.

- Before opening a pull request, verify the scope doesn't overlap with linked or sibling issues; if uncertain, ask in the issue first.
- If an issue is blocked by unreleased prerequisites, propose deferral with `blocked-by` edges rather than writing early code.
- Revert any out-of-scope incidental edits before requesting review. Lint and format changes that touch untouched files in the diff get reverted.

The `issue-work` skill already encodes most of this; this file exists to remind Claude Code to apply it without having to call the skill.
