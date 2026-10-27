#!/usr/bin/env bash
# Times each sim in Goblin (1 and 4 worker threads) and in Mesa.
# Usage: bench/vs_mesa/run.sh [goblin-binary] [python-with-mesa]
# Mesa's time leaves out importing Mesa (about a second).
set -u
G=${1:-goblin}
P=${2:-python3}
D=$(cd "$(dirname "$0")" && pwd)
S="$D/../../sims"
secs() { local s e; s=$(date +%s.%N); "$@" >/dev/null 2>&1; e=$(date +%s.%N); echo "$e - $s" | bc; }
mesa() { "$P" -c "
import runpy, sys, time, os, io, contextlib
sys.argv = ['$1']
import mesa
t = time.perf_counter()
with contextlib.redirect_stdout(io.StringIO()):
    runpy.run_path('$D/$1', run_name='__main__')
print(round(time.perf_counter() - t, 2))"; }
row() { # name, env, goblin file, mesa file
  local name=$1 envs=$2 gf=$3 mf=$4
  local g1 g4 m
  g1=$(secs env $envs GOBLIN_ACTOR_THREADS=1 "$G" run "$S/$gf")
  g4=$(secs env $envs GOBLIN_ACTOR_THREADS=4 "$G" run "$S/$gf")
  m=$(env $envs bash -c "$(declare -f mesa); D='$D'; P='$P'; mesa $mf")
  printf "%-34s goblin 1 thread %6.2f s   4 threads %6.2f s   mesa %6.2f s\n" "$name" "$g1" "$g4" "$m"
}
row "epidemic 4x100, 120 days"        "DAYS=120"             epidemic/plague.gbln        epidemic.py
row "epidemic 4x1000, 60 days"        "PEOPLE=1000 DAYS=60"  epidemic/plague.gbln        epidemic.py
row "predator-prey 4 meadows, 360 d"  "DAYS=360"             predator_prey/meadows.gbln  predator_prey.py
row "feudal 3 realms, 200 years"      "YEARS=200"            feudal/kingdom.gbln         feudal.py
