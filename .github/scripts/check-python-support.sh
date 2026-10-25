#!/usr/bin/env bash
# Report whether ci.yml tests every CPython release upstream supports and
# requires-python floors at the oldest of them.
#
# Writes `drift=<bool>` to $GITHUB_OUTPUT and a create-an-issue body to
# $ISSUE_FILE. Run from the repository root.
#
# Aborts rather than reporting drift when a source will not parse, so a
# renamed field cannot read as "nothing tested".
set -euo pipefail

# v1 exposes a computed `isEol`. v0 gives Python a date in `eol`, which
# would mean date arithmetic against the runner clock.
readonly ENDOFLIFE_API="https://endoflife.date/api/v1/products/python"

readonly MATRIX_JOB="python-tests"

readonly CI_WORKFLOW=".github/workflows/ci.yml"
readonly PYPROJECT="pyproject.toml"

: "${GITHUB_OUTPUT:=/dev/stdout}"
: "${ISSUE_FILE:=python-drift-issue.md}"

die() {
  echo "$1; aborting" >&2
  exit 1
}

require_versions() {
  local source=$1 versions=$2
  [ -n "$versions" ] || die "$source named no Python versions"
}

supported_releases() {
  curl -fsSL "$ENDOFLIFE_API" \
    | jq -r '.result.releases | map(select(.isEol == false)) | .[].name' \
    | sort -V
}

# The `3.x` job tracks whatever ships next, so comparing it would always
# report drift.
matrix_versions() {
  yq ".jobs.\"$MATRIX_JOB\".strategy.matrix.python-version[]" "$CI_WORKFLOW" | sort -V
}

# requires-python is PEP 621, so reading it survives a change of packaging
# tool.
python_floor() {
  grep -oE '^requires-python = ">=[0-9]+\.[0-9]+' "$PYPROJECT" \
    | grep -oE '[0-9]+\.[0-9]+$'
}

as_list() {
  printf '%s' "$1" | paste -sd, -
}

supported=$(supported_releases)
require_versions "endoflife.date" "$supported"

tested=$(matrix_versions)
require_versions "$CI_WORKFLOW job $MATRIX_JOB" "$tested"

floor=$(python_floor)
[ -n "$floor" ] || die "could not read requires-python from $PYPROJECT"

oldest=$(head -n 1 <<< "$supported")

drift=false
[ "$supported" = "$tested" ] || drift=true
[ "$floor" = "$oldest" ] || drift=true

echo "drift=$drift" >> "$GITHUB_OUTPUT"

# create-an-issue reads the title and labels from front matter.
{
  echo "---"
  echo "title: Supported Python versions out of date"
  echo "labels:"
  echo "  - python"
  echo "---"
  echo "The CI matrix should name every CPython release upstream"
  echo "still supports, and \`requires-python\` should floor at the"
  echo "oldest of them, per the comment above the matrix in"
  echo "\`$CI_WORKFLOW\`."
  echo
  echo "| Source | Versions |"
  echo "| --- | --- |"
  echo "| Expected (endoflife.date, not end-of-life) | $(as_list "$supported") |"
  echo "| \`$MATRIX_JOB\` matrix | $(as_list "$tested") |"
  echo "| \`requires-python\` floor | $floor |"
  echo
  echo "Raising the floor drops users on that release, so it ships as a"
  echo "breaking change."
} > "$ISSUE_FILE"
