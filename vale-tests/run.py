"""Validate Vale rules and run fixture-based tests without requiring vale CLI.

Usage:
    python vale-tests/run.py          # validate rules + run all fixtures
    python vale-tests/run.py --check  # rules-only, no fixtures
"""

import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
STYLES_DIR = ROOT / "styles" / "Custom-Agent"
FIXTURES_JSON = Path(__file__).resolve().parent / "fixtures.json"
SENTENCE_CHAR_LIMIT = 180


def validate_rule_yaml(path: Path) -> bool:
    content = path.read_text(encoding="utf-8")
    required = ["extends:", "message:", "level:"]
    ok = all(field in content for field in required)
    if not ok:
        print(f"  FAIL: missing required field in {path.name}")
        return False
    return True


def validate_rules() -> int:
    errors = 0
    print(f"  STYLES_DIR: {STYLES_DIR}")
    for yml_file in sorted(STYLES_DIR.glob("*.yml")):
        print(f"  Rule: {yml_file.name} ", end="")
        if not yml_file.is_file():
            continue
        try:
            if validate_rule_yaml(yml_file):
                print("VALID")
            else:
                errors += 1
        except Exception:
            print("PARSE_ERROR")
            errors += 1
    return errors


def strip_code_blocks(text: str) -> str:
    in_block = False
    lines = []
    for line in text.split("\n"):
        stripped = line.strip()
        if stripped.startswith("```"):
            in_block = not in_block
            continue
        if not in_block:
            lines.append(line)
    return "\n".join(lines)


def run_sentence_length_check(text: str) -> list[tuple[int, str]]:
    text = strip_code_blocks(text)
    findings = []
    sentences = re.split(r"(?<=[.!?])\s+", text)
    col = 0
    for s in sentences:
        line = text[:col].count("\n") + 1
        actual_char_count = len(s)
        if actual_char_count > SENTENCE_CHAR_LIMIT:
            snippet = s[:60].replace("\n", " ") + "..."
            findings.append((line, snippet))
        col += len(s) + 1
    return findings


def load_fixtures() -> dict:
    if not FIXTURES_JSON.exists():
        print(f"  ERROR: {FIXTURES_JSON} not found", file=sys.stderr)
        return {}
    with open(FIXTURES_JSON, encoding="utf-8") as f:
        data = json.load(f)
    return data.get("fixtures", {})


def test_fixture_content(name: str, text: str, expect_length: bool) -> tuple[str, bool, int]:
    length_findings = run_sentence_length_check(text)
    nl = len(length_findings)
    length_ok = (nl > 0) == expect_length
    return (name, length_ok, nl)


def main() -> int:
    check_only = "--check" in sys.argv

    print("=== VALE RULES VALIDATION ===")
    rule_errors = validate_rules()

    if check_only:
        return rule_errors

    print(f"\n=== VALE FIXTURE TESTS (sentence limit: {SENTENCE_CHAR_LIMIT} chars) ===")

    fixtures = load_fixtures()
    if not fixtures:
        print("  ERROR: no fixtures loaded")
        return 1

    total = 0
    passed = 0
    for name, fixture in fixtures.items():
        result = test_fixture_content(name, fixture["content"], fixture["expect_length"])
        _, ok, nl = result
        total += 1
        desc = fixture.get("description", name)
        if ok:
            passed += 1
            print(f"  PASS: {name} ({desc})")
        else:
            print(f"  FAIL: {name} ({desc}) - len_findings={nl} exp={fixture['expect_length']}")
            print(f"         NOTE: fixture-based rule simulation; full vale CLI test requires vale v3.13.0")

    print(f"\n  Result: {passed}/{total} passed, {rule_errors} rule errors")
    return 0 if (passed == total and rule_errors == 0) else 1


if __name__ == "__main__":
    sys.exit(main())
