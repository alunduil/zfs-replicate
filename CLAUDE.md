# zfs-replicate—Claude Code guide

## Session defaults

`.claude/settings.json` ships committed defaults for Claude Code sessions on
this repo. A `SessionStart` hook runs `poetry install --quiet && pre-commit
install --quiet --install-hooks`, so the environment and git hooks are ready
before the first turn. The permission allowlist pre-approves read-only
checks—`poetry run pytest`, `pre-commit run` (black, isort, flake8, pylint, bandit,
mypy, and the rest of the pre-commit stack), `poetry run zfs-replicate --help`,
and read-only git (`diff`, `log`, `status`, `show`). Put machine-specific
overrides in `.claude/settings.local.json` (gitignored).
