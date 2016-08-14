source scripts/lib.sh

print_colored_text BLUE "Actual code:\n"
cloc ./safepath/src

print_colored_text BLUE "Tests:\n"
cloc ./safepath-testing/src ./safepath-testing/test
