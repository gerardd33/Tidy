#!/bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

if [ ! -f "${WORKING_DIR}"/Makefile ]; then
    echo "Error: Make sure to run prepare_syntax_test_env before running any tests."
    exit 1
fi

# Build parser
make -C "${WORKING_DIR}"

"${WORKING_DIR}"/SyntaxTest/Tidy/Test < "${WORKING_DIR}"/Test.ty
