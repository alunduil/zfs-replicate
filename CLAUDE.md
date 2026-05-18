# Claude.md

Repository-specific guidance for Claude Code working in `zfs-replicate`. Keep this short, with repo-specific guidance.

## Tools inventory

Before reaching for `curl`, hand-rolled API calls, or first-principles scripts, see whether one of these already covers the task:

- **Package manager**: Poetry. Manifest in `pyproject.toml`, lock in `poetry.lock`, project-local configuration in `poetry.toml`. Use `poetry run <cmd>` to enter the project's virtual environment.
- **Development shell**: Nix via `shell.nix` (which sources `nix/shell.nix`). `direnv` reads `.envrc`, and `.devcontainer/` keeps the GitHub Codespaces and Visual Studio Code Remote Containers configuration in sync.
- **Tests**: `pytest` configured under `[tool.pytest.ini_options]` in `pyproject.toml` (`--doctest-modules --cov=zfs --cov-report=term-missing`, `testpaths = ["zfs_test"]`). Doctest coverage is part of the suite, so don't break docstrings. Hypothesis is available.
- **Lint, format, type, and prose**: All wired through `pre-commit` (`.pre-commit-config.yaml`). The bundle covers `black`, `isort`, `flake8` (`.flake8`), `bandit`, `pylint` (`.pylintrc`), `pydocstyle`, `mypy` (`mypy.ini`), and Vale prose linting (`.vale.ini`, with custom rules under `styles/`). Run `poetry run pre-commit run --all-files` before requesting review.
- **Entry point**: `zfs-replicate = "zfs.replicate.cli.main:main"` (registered in `pyproject.toml`'s `[tool.poetry.scripts]`).
- **Source layout**: implementation under `zfs/replicate/`, tests under `zfs_test/replicate_test/`.

## Scope discipline

This repository has five open release milestones (`4.2.0` through `5.0.0`) and uses a `blocked` label to sequence work. Pull requests that bleed into sibling issues or land code for a milestone whose prerequisites haven't shipped have caused friction in past sessions. Before opening a pull request:

- Verify the change doesn't overlap with linked or sibling issues. If uncertain, ask in the issue rather than guess.
- If the issue is blocked by unshipped prerequisites (look for the `blocked` label or `blocked-by` references), propose deferral via a comment rather than writing premature code.
- Revert out-of-scope incidental edits (formatting drive-bys, unrelated refactors) before requesting review, so the diff stays matched to the issue's acceptance criteria.

Most of the above is also encoded in the user-level `issue-work` skill; this file exists so the discipline applies even when that skill isn't loaded.
