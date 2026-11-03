#!/usr/bin/env bash
# Checks for capstone steps 1 and 2. Usage: sims/capstone/check.sh [goblin-binary]
# Runs 3,000 years and checks: the output matches expected.txt, two runs
# match, 1 thread matches 4 threads, river valleys fill first, the
# population levels off, kingdoms fall, and the distance penalty caps how
# big a kingdom gets (against a run without it).
set -u
B=${1:-goblin}
case $B in */*) B=$(cd "$(dirname "$B")" && pwd)/$(basename "$B") ;; esac
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { (cd "$D" && env "$@" "$B" run world.gbln 2>&1 | grep -v ' → exit ' | grep -v '^time:'); }
run YEARS=3000 GOBLIN_ACTOR_THREADS=4 > "$T/a"
run YEARS=3000 GOBLIN_ACTOR_THREADS=4 > "$T/b"
run YEARS=3000 GOBLIN_ACTOR_THREADS=1 > "$T/c"
run YEARS=3000 REACH=1000 > "$T/far"
largest() { awk '/^check: largest kingdom ever:/ { print $5 }' "$1"; }
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected.txt";      check diff "$T/a" "$D/expected.txt"
LABEL="two runs are identical";    check diff "$T/a" "$T/b"
LABEL="1 thread = 4 threads";      check diff "$T/a" "$T/c"
LABEL="river valleys fill first";  check grep -q "^check: first to fill: river$" "$T/a"
LABEL="population levels off";     check awk '/^check: growth/ { g = $NF; sub("%", "", g); exit !(g + 0 < 1) }' "$T/a"
LABEL="kingdoms fall";             check awk '/^check: kingdoms fallen:/ { exit !($4 + 0 > 0) }' "$T/a"
LABEL="distance caps a kingdom ($(largest "$T/a") provinces, $(largest "$T/far") without the penalty)"
check test "$(largest "$T/a")" -lt "$(largest "$T/far")"
rm -f "$D/capstone_metrics.csv"
exit $fail
