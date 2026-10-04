#!/usr/bin/env bash
# Render a failed dependency-ceiling run as a create-an-issue body.
#
# Run after `poetry update`. Before it, `poetry show` reports the locked
# versions, not the ones that failed.
set -euo pipefail

: "${ISSUE_FILE:=dependency-ceiling-issue.md}"

run_url="${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"

# create-an-issue finds the issue to refresh by title, so the title has to stay
# stable or each failing night opens another one.
{
  echo "---"
  echo "title: Newest dependency releases fail the suite"
  echo "labels:"
  echo "  - dependencies"
  echo "---"
  echo "The nightly \`Test the newest dependency releases\` job failed"
  echo "against the newest runtime releases pyproject.toml allows:"
  echo
  echo '```text'
  poetry show --only main --top-level
  echo '```'
  echo
  echo "See [the failing run]($run_url). Fix the incompatibility, or"
  echo "lower the ceiling in pyproject.toml if the release drops"
  echo "something this project needs."
} > "$ISSUE_FILE"
