#!/usr/bin/env bash
# Deterministic checks for the town sim. Usage: sims/town/check.sh [goblin-binary]
# Runs 120 days and checks: the output matches expected-120.txt, two runs match,
# 1 thread matches 4 threads, the actor run matches the serial run, the money
# total never drifts, and the rebuilt-from-history districts match the live run.
set -u
B=${1:-goblin}
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { env TOWN_DAYS=120 "$@" "$B" run "$D/town.gbln" 2>&1 | grep -v ' → exit \|mode ' ; }
run GOBLIN_ACTOR_THREADS=4 > "$T/a"
run GOBLIN_ACTOR_THREADS=4 > "$T/b"
run GOBLIN_ACTOR_THREADS=1 > "$T/c"
run TOWN_MODE=serial > "$T/s"
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected-120.txt";   check diff "$T/a" "$D/expected-120.txt"
LABEL="two runs are identical";     check diff "$T/a" "$T/b"
LABEL="1 thread = 4 threads";       check diff "$T/a" "$T/c"
LABEL="actors = serial";            check diff "$T/a" "$T/s"
LABEL="money never drifts";         check bash -c "! grep -q 'should be' '$T/a'"
LABEL="rebuilds match the live run"; check bash -c "grep -q 'from history: true' '$T/a' && ! grep -q 'from history: false' '$T/a'"
exit $fail
