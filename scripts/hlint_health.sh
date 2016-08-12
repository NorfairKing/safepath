source scripts/lib.sh
h () {
  hlint \
    ./safepath/src \
    --ignore "Use fromMaybe"

  hlint \
    ./safepath-testing/src \
    ./safepath-testing/test \
    --ignore "Redundant do" \
    --ignore "Redundant $"
}
check "Hlint" h
