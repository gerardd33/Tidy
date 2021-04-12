#!/bin/bash

../build_parser.sh
mkdir -p tests_output


function run_tests_for_directory {
    DIRECTORY=$1
    
    for INPUT in $(ls $DIRECTORY); do
        echo && echo "Test: $INPUT :"
        
        cat $DIRECTORY/"$INPUT" > Test.ty
        OUTPUT_FILE_NAME="${INPUT%%.*}".out
        
        ./single_test.sh > ./tests_output/"$OUTPUT_FILE_NAME"
        
        cat ./tests_output/"$OUTPUT_FILE_NAME" | grep -e "Parse Successful!" -e "Failed..."
    done
}

echo "Bad syntax examples - Expecting failure"
run_tests_for_directory ./tests_input/unit/bad

echo "Good syntax examples - Expecting success:"
run_tests_for_directory ./tests_input/unit/good

echo && echo 

run_tests_for_directory ./tests_input/integration

echo && echo
