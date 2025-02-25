set -e

export LANG="C.UTF-8"

cabal bench --benchmark-options \
  '+RTS -T -RTS
  -t100
  --csv bench/app/bench.csv
  --svg bench/app/bench.svg
  --baseline bench/app/baseline_9.10.1.csv
  --fail-if-slower 20
  --fail-if-faster 20'
