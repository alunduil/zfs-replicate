#!/usr/bin/env bash
# Render a failed dependency-ceiling run as a create-an-issue body.
#
# Fills $ISSUE_FILE, whose front matter carries the title create-an-issue
# matches on, with the runtime versions Poetry resolved and a link to the run.
# The path takes an override. Run from the repository root after
# `poetry update`.
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
