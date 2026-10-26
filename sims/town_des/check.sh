#!/usr/bin/env bash
# Deterministic checks for the town on DES. Usage: sims/town_des/check.sh [goblin-binary]
# Runs 60 days and checks: the output matches expected-60.txt, two runs match,
# 1 thread matches 4 threads, and with one district (nothing in flight between
# districts, nobody dies in 60 days) the money total never changes.
set -u
B=${1:-goblin}
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { env TOWN_DAYS=60 "$@" "$B" run "$D/town.gbln" 2>&1 | grep -v ' → exit ' ; }
run GOBLIN_ACTOR_THREADS=4 > "$T/a"
run GOBLIN_ACTOR_THREADS=4 > "$T/b"
run GOBLIN_ACTOR_THREADS=1 > "$T/c"
run TOWN_DISTRICTS=1 > "$T/one"
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected-60.txt";     check diff "$T/a" "$D/expected-60.txt"
LABEL="two runs are identical";      check diff "$T/a" "$T/b"
LABEL="1 thread = 4 threads";        check diff "$T/a" "$T/c"
LABEL="one district keeps every coin"; check bash -c "grep -q '; 0 left with the dead or is in flight' '$T/one'"
exit $fail
