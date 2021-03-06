#!/bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

# Build Tidy
(cd ../../.. && ./build_tidy.sh)

ERRORS=0

function run_tests_for_directory {
    DIRECTORY=${1}
    EXPECTED_OUTPUT_FILE_NAME="expected.out"
    ACTUAL_OUTPUT_FILE_NAME="actual.out"

    for INPUT in $(ls "${DIRECTORY}"/input); do
        echo && printf "Test: %s: " "${INPUT}"
        OUTPUT="${INPUT%_*}".txt

        cat "${DIRECTORY}"/input/"${INPUT}" > "${WORKING_DIR}"/Test.ty
        cat "${DIRECTORY}"/output/"${OUTPUT}" > "${WORKING_DIR}"/"${EXPECTED_OUTPUT_FILE_NAME}"

        (cd ../../.. && ./tidy "${WORKING_DIR}"/Test.ty > "${WORKING_DIR}"/"${ACTUAL_OUTPUT_FILE_NAME}" 2>> "${WORKING_DIR}"/"${ACTUAL_OUTPUT_FILE_NAME}")
        RESULT=$(diff -q "${WORKING_DIR}"/"${ACTUAL_OUTPUT_FILE_NAME}" "${WORKING_DIR}"/"${EXPECTED_OUTPUT_FILE_NAME}")

        if [ -z "${RESULT}" ]; then
            tput setaf 2 && tput bold && echo OK && tput sgr0
        else
            tput setaf 1 && tput bold && echo ERROR! && tput sgr0
            ((ERRORS++))
        fi
    done
    echo
}

run_tests_for_directory "${WORKING_DIR}"/good
run_tests_for_directory "${WORKING_DIR}"/bad

if [ ${ERRORS} -eq 0 ]; then
    echo "All tests passed successfully."
elif [ ${ERRORS} -eq 1 ]; then
    echo "1 test failed."
else
    echo "${ERRORS} tests failed."
fi
echo
