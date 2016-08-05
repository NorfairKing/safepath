source scripts/lib.sh
h () {
  hlint ./safepath/src

  hlint ./safepath/test \
    --ignore "Redundant do" \
    --ignore "Redundant $"
}
check "Hlint" h
