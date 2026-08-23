#!/usr/bin/env bash
# Render the mutation score as a Markdown table on the job summary.
#
# Reads the JSON `mutmut export-cicd-stats` writes and appends to
# $GITHUB_STEP_SUMMARY. Both paths take an override, so a local run prints the
# same table to stdout. Run from the repository root.
set -euo pipefail

: "${STATS_FILE:=mutants/mutmut-cicd-stats.json}"
: "${GITHUB_STEP_SUMMARY:=/dev/stdout}"

die() {
  echo "$1; aborting" >&2
  exit 1
}

# jq reports a missing file as a parse error, which reads as a broken script
# rather than a run that stopped before exporting.
[ -f "$STATS_FILE" ] || die "$STATS_FILE is missing; mutmut export-cicd-stats writes it"

read -r killed survived no_tests timed_out total < <(
  jq -r '[.killed // 0, .survived // 0, .no_tests // 0, .timeout // 0, .total // 0] | @tsv' \
    "$STATS_FILE"
)

# Mutants no test reaches are a coverage gap, so the percentage divides by what
# a test actually ran.
reached=$((killed + survived))

{
  echo "| Verdict | Mutants |"
  echo "| --- | --- |"
  echo "| Killed | $killed |"
  echo "| Survived | $survived |"
  echo "| Unreached by any test | $no_tests |"
  echo "| Timed out | $timed_out |"
  echo "| Total | $total |"
  echo
  if [ "$reached" -eq 0 ]; then
    echo "No mutant reached a test."
  else
    # Shell arithmetic truncates where the score should round.
    percent=$(awk -v killed="$killed" -v reached="$reached" \
      'BEGIN { printf "%.0f", 100 * killed / reached }')
    echo "Killed $percent% of the $reached mutants a test reached."
  fi
} >> "$GITHUB_STEP_SUMMARY"
