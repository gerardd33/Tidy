#!/bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

if [ ! -f "${WORKING_DIR}"/Makefile ]; then
    echo "Error: Make sure to run prepare_syntax_test_env before running any tests."
    exit 1
fi


# Build parser
make -C "${WORKING_DIR}"

function run_tests_for_directory {
    DIRECTORY=${1}
    if [ "${2}" = false ]; then
      EXPECTED="Parse              Failed..."
    else
      EXPECTED="Parse Successful!"
    fi

    for INPUT in $(ls ${DIRECTORY}); do
        echo && printf "Test: %s: " "${INPUT}"
        OUTPUT="${WORKING_DIR}"/parsing.out

        cat "${DIRECTORY}"/"${INPUT}" > "${WORKING_DIR}"/Test.ty
        "${WORKING_DIR}"/single_test.sh > "${OUTPUT}"

        RESULT="$(grep -e "Parse Successful!" -e "Failed..." "${OUTPUT}")"

        if [ "${RESULT}" = "${EXPECTED}" ]; then
            tput setaf 2 && tput bold && echo OK && tput sgr0
        else
            tput setaf 1 && tput bold && echo ERROR! && tput sgr0
            notify-send 'Wrong Answer'
            exit 1
        fi
    done
    echo
}


run_tests_for_directory "${WORKING_DIR}"/good true
run_tests_for_directory "${WORKING_DIR}"/bad false

echo "All tests passed successfully." && echo
