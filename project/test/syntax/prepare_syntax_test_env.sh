#! /bin/bash

WORKING_DIR=$(dirname "${0}")

mkdir -p "${WORKING_DIR}"/tests_output

bnfc --haskell -d -m "${WORKING_DIR}"/../../src/Parser/Tidy.cf -o "${WORKING_DIR}" -p "SyntaxTest" && \
echo && echo "Test environment prepared." && echo


RED=$(tput setaf 1)
RESET=$(tput sgr0)

echo "${RED}IMPORTANT: This has created a test environment with large generated files. Make sure to clean up once you're done by running ./clear_syntax_test_env.sh.${RESET}"
