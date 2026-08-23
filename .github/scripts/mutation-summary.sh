#!/usr/bin/env bash
# Render the mutation score as a create-an-issue body.
#
# Reads the JSON `mutmut export-cicd-stats` writes and fills $ISSUE_FILE, whose
# front matter carries the title create-an-issue matches on. Both paths take an
# override. Run from the repository root.
set -euo pipefail

: "${STATS_FILE:=mutants/mutmut-cicd-stats.json}"
: "${ISSUE_FILE:=mutation-score-issue.md}"

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

# create-an-issue finds the issue to refresh by title, so the title has to stay
# stable or each run opens another one.
{
  echo "---"
  echo "title: Mutation score"
  echo "---"
  echo "The nightly \`Score mutant detection\` job rewrites this issue after each"
  echo "sweep. Any work it implies is filed separately."
  echo
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
} > "$ISSUE_FILE"
