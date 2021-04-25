#!/bin/bash

WORKING_DIR=$(dirname ${0})

if [ ! -f ${WORKING_DIR}/Makefile ]; then
    echo "Error: Make sure to run prepare_syntax_test_env before running any tests."
    exit 1
fi


# Build parser
make -C ${WORKING_DIR}


function run_tests_for_directory {
    DIRECTORY=${1}
    
    for INPUT in $(ls ${DIRECTORY}); do
        echo && echo "Test: ${INPUT} :"
        
        cat ${DIRECTORY}/${INPUT} > ${WORKING_DIR}/Test.ty
        OUTPUT_FILE_NAME=${INPUT%%.*}.out
        
        ${WORKING_DIR}/single_test.sh > ${WORKING_DIR}/tests_output/${OUTPUT_FILE_NAME}
        
        cat ${WORKING_DIR}/tests_output/${OUTPUT_FILE_NAME} | grep -e "Parse Successful!" -e "Failed..."
    done
    
    echo && echo
}


echo "Bad syntax examples - Expecting failure"
run_tests_for_directory ${WORKING_DIR}/tests_input/unit/bad

echo "Good syntax examples - Expecting success:"
run_tests_for_directory ${WORKING_DIR}/tests_input/unit/good

run_tests_for_directory ${WORKING_DIR}/tests_input/integration
