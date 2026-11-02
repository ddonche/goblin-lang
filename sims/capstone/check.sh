#!/usr/bin/env bash
# Checks for capstone step 1. Usage: sims/capstone/check.sh [goblin-binary]
# Runs 3,000 years and checks: the output matches expected.txt, two runs
# match, river valleys fill first, and the population levels off.
set -u
B=${1:-goblin}
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { (cd "$D" && env "$@" "$B" run world.gbln 2>&1 | grep -v ' → exit ' | grep -v '^time:'); }
run YEARS=3000 > "$T/a"
run YEARS=3000 > "$T/b"
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected.txt";      check diff "$T/a" "$D/expected.txt"
LABEL="two runs are identical";    check diff "$T/a" "$T/b"
LABEL="river valleys fill first";  check grep -q "^check: first to fill: river$" "$T/a"
LABEL="population levels off";     check awk '/^check: growth/ { g = $NF; sub("%", "", g); exit !(g + 0 < 1) }' "$T/a"
rm -f "$D/capstone_metrics.csv"
exit $fail
