#!/usr/bin/env bash
# Report whether ci.yml still tests every CPython release upstream supports.
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

# ci.yml duplicates the matrix across these jobs (#616).
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

# The `3.x` leg sits under `include:`, so yq leaves it out. It tracks
# whatever ships next by design and would always read as drift.
matrix_versions() {
  yq ".jobs.\"$1\".strategy.matrix.python-version[]" "$CI_WORKFLOW" | sort -V
}

# The floor is reported for context, never compared: raising it drops users,
# so it cannot be part of the `ci` fix this check asks for. requires-python
# is PEP 621, so reading it survives a change of packaging tool.
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
