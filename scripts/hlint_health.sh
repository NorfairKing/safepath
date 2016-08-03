source scripts/lib.sh
h () {
  hlint ./src

  hlint ./test \
    --ignore "Redundant do" \
    --ignore "Redundant $"
}
check "Hlint" h
