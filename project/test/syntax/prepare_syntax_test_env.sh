#! /bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

bnfc --haskell -d -m "${WORKING_DIR}"/../../src/Parser/Tidy.cf -o "${WORKING_DIR}" -p "SyntaxTest" && \
echo && echo "Test environment prepared. This has created a test environment with generated files including binaries." && echo && echo "Make sure to clean up once you're done by running ./clear_syntax_test_env.sh."
