#! /bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

rm -rf "${WORKING_DIR}"/SyntaxTest "${WORKING_DIR}"/Makefile "${WORKING_DIR}"/tests_output && \
echo "Test environment cleared."
