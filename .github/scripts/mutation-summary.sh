#!/usr/bin/env bash
# Render the mutation score as a Markdown table on the job summary.
#
# Reads the JSON `mutmut export-cicd-stats` writes and appends to
# $GITHUB_STEP_SUMMARY. Run from the repository root; outside Actions it
# prints to stdout, so `mutmut run && mutmut export-cicd-stats &&
# .github/scripts/mutation-summary.sh` reproduces what the job publishes.
#
# Reports without judging: the exit status says whether the summary could be
# written, never whether the score was high enough. A threshold is #485's
# stated follow-up.
set -euo pipefail

: "${STATS_FILE:=mutants/mutmut-cicd-stats.json}"
: "${GITHUB_STEP_SUMMARY:=/dev/stdout}"

die() {
  echo "$1; aborting" >&2
  exit 1
}

# jq would report a missing file as a parse error, which reads as a broken
# script rather than a run that never got far enough to export.
[ -f "$STATS_FILE" ] || die "$STATS_FILE is missing; mutmut export-cicd-stats writes it"

read -r killed survived no_tests timed_out total < <(
  jq -r '[.killed // 0, .survived // 0, .no_tests // 0, .timeout // 0, .total // 0] | @tsv' \
    "$STATS_FILE"
)

# Mutants no test reaches are a coverage gap, not a suite that failed to
# notice, so the percentage divides by what a test actually ran.
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
    # Shell arithmetic truncates, which would report 59.9% as 59%.
    percent=$(awk -v killed="$killed" -v reached="$reached" \
      'BEGIN { printf "%.0f", 100 * killed / reached }')
    echo "Killed $percent% of the $reached mutants a test reached."
  fi
} >> "$GITHUB_STEP_SUMMARY"
