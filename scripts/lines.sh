source scripts/lib.sh

print_colored_text BLUE "Actual code:\n"
cloc ./src

print_colored_text BLUE "Tests:\n"
cloc ./test
