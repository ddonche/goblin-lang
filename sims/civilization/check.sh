#!/usr/bin/env bash
# Deterministic checks for the civilization sim. Usage: sims/civilization/check.sh [goblin-binary]
# Runs 120 months and checks: the output matches expected.txt, two runs match,
# and 1 thread matches 4 threads.
set -u
B=${1:-goblin}
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { env MONTHS=120 "$@" "$B" run "$D/villages.gbln" 2>&1 | grep -v ' → exit ' ; }
run GOBLIN_ACTOR_THREADS=4 > "$T/a"
run GOBLIN_ACTOR_THREADS=4 > "$T/b"
run GOBLIN_ACTOR_THREADS=1 > "$T/c"
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected.txt";   check diff "$T/a" "$D/expected.txt"
LABEL="two runs are identical"; check diff "$T/a" "$T/b"
LABEL="1 thread = 4 threads";   check diff "$T/a" "$T/c"
exit $fail
