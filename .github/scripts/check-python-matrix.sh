#!/usr/bin/env bash
# Report whether ci.yml still tests every CPython release upstream supports.
#
# Writes `drift=<bool>` to $GITHUB_OUTPUT and, when they disagree, a
# create-an-issue body to $ISSUE_FILE. Run from the repository root.
#
# Aborts instead of reporting drift when a source will not parse, so a
# renamed field can never read as "no versions tested" and file a bogus
# issue against an empty set.
set -euo pipefail

# v1 computes `isEol` server-side. v0 reports Python's `eol` as a date
# rather than the `false` that collection-json.hs keys on for GHC, so v1
# avoids doing date arithmetic against the runner clock.
readonly ENDOFLIFE_API="https://endoflife.date/api/v1/products/python"

# Each job carries its own copy of the matrix until #616 collapses them, so
# every copy is checked and every copy appears in the issue.
readonly MATRIX_JOBS=(python-tests cli-entry-point)

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

# The `3.x` leg sits under `include:`, so yq leaves it out. That leg tracks
# whatever ships next by design and would always read as drift.
matrix_versions() {
  yq ".jobs.\"$1\".strategy.matrix.python-version[]" "$CI_WORKFLOW" | sort -V
}

# requires-python is PEP 621, so this survives #619's uv migration in a way
# that reading a [tool.poetry] key would not.
python_floor() {
  grep -oE '^requires-python = ">=[0-9]+\.[0-9]+' "$PYPROJECT" \
    | grep -oE '[0-9]+\.[0-9]+$'
}

as_list() {
  printf '%s' "$1" | paste -sd, -
}

supported=$(supported_releases)
require_versions "endoflife.date" "$supported"

floor=$(python_floor)
[ -n "$floor" ] || die "could not read requires-python from $PYPROJECT"

# The floor is reported but never triggers: raising it drops users and is
# breaking, so it belongs to #398 rather than to a `ci` fix that would
# close this issue.
declare -A tested
drift=false
for job in "${MATRIX_JOBS[@]}"; do
  tested[$job]=$(matrix_versions "$job")
  require_versions "$CI_WORKFLOW job $job" "${tested[$job]}"
  [ "$supported" = "${tested[$job]}" ] || drift=true
done

echo "drift=$drift" >> "$GITHUB_OUTPUT"

# create-an-issue reads the title and labels from front matter.
{
  echo "---"
  echo "title: Python version matrix out of date"
  echo "labels:"
  echo "  - python"
  echo "---"
  echo "The CI matrices should name every CPython release upstream"
  echo "still supports, per the comment above the matrix in"
  echo "\`$CI_WORKFLOW\`."
  echo
  echo "| Source | Versions |"
  echo "| --- | --- |"
  echo "| Expected (endoflife.date, not end-of-life) | $(as_list "$supported") |"
  for job in "${MATRIX_JOBS[@]}"; do
    echo "| \`$job\` matrix | $(as_list "${tested[$job]}") |"
  done
  echo
  echo "Update both matrices in the same \`ci\` change."
  echo
  echo "\`requires-python\` currently floors at $floor. Raising it drops"
  echo "users still on that release, so it is breaking and belongs to"
  echo "#398, not here."
} > "$ISSUE_FILE"
