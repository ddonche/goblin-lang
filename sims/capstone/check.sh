#!/usr/bin/env bash
# Checks for capstone steps 1 and 2. Usage: sims/capstone/check.sh [goblin-binary]
# Step 1 (world.gbln) runs 3,000 years and checks: the output matches
# expected.txt, two runs match, river valleys fill first, and the
# population levels off. Step 2 (history.gbln) runs 1,200 years of
# prehistory and 500 of history and checks: the output matches
# expected_history.txt, two runs match (output and event log), every
# decision is taken, and wars start and end. It reports the balance
# numbers (polities left, the largest one's share of the land, years with
# a war under way) without judging them.
set -u
B=${1:-goblin}
case $B in */*) B=$(cd "$(dirname "$B")" && pwd)/$(basename "$B") ;; esac
D=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
run() { f=$1; shift; (cd "$D" && env "$@" "$B" run "$f" 2>&1 | grep -v ' → exit ' | grep -v '^time:'); }
run world.gbln YEARS=3000 > "$T/a"
run world.gbln YEARS=3000 > "$T/b"
run history.gbln YEARS=500 > "$T/h1"; cp "$D/history_events.txt" "$T/e1"
run history.gbln YEARS=500 > "$T/h2"; cp "$D/history_events.txt" "$T/e2"
fail=0
check() { if "$@" >/dev/null; then echo "ok   $LABEL"; else echo "FAIL $LABEL"; fail=1; fi; }
LABEL="matches expected.txt";      check diff "$T/a" "$D/expected.txt"
LABEL="two runs are identical";    check diff "$T/a" "$T/b"
LABEL="river valleys fill first";  check grep -q "^check: first to fill: river$" "$T/a"
LABEL="population levels off";     check awk '/^check: growth/ { g = $NF; sub("%", "", g); exit !(g + 0 < 1) }' "$T/a"
LABEL="history matches expected_history.txt"; check diff "$T/h1" "$D/expected_history.txt"
LABEL="two histories are identical";          check diff "$T/h1" "$T/h2"
LABEL="two event logs are identical";         check cmp "$T/e1" "$T/e2"
for d in tax relieve build levy disband borrow trade envoy aid ally treaty spy denounce embargo demand show_of_force raid strike war break_treaty sue_for_peace; do
    LABEL="decision taken: $d ($(grep -c " | $d | " "$T/e1"))"; check grep -q " | $d | " "$T/e1"
done
LABEL="wars end ($(grep -c '| War .* | erase$' "$T/e1") of $(grep -c ' | war | ' "$T/e1"))"; check grep -q '| War .* | erase$' "$T/e1"
echo "     $(tail -n 1 "$D/history_metrics.csv" | awk -F, '{ printf "year %s: %s polities, the largest holds %s provinces (%.1f%% of the held land)", $1, $7, $15, $16 * 100 }')"
echo "     $(grep '^check: share of polity-years' "$T/h1" | sed 's/^check: //')"
rm -f "$D/capstone_metrics.csv" "$D/history_metrics.csv" "$D/history_events.txt"
exit $fail
