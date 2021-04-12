#!/bin/bash

WORKING_DIR=`dirname "$0"`
"$WORKING_DIR"/../build_parser.sh
mkdir -p "$WORKING_DIR"/tests_output


function run_tests_for_directory {
    DIRECTORY=$1
    
    for INPUT in $(ls $DIRECTORY); do
        echo && echo "Test: $INPUT :"
        
        cat $DIRECTORY/"$INPUT" > "$WORKING_DIR"/Test.ty
        OUTPUT_FILE_NAME="${INPUT%%.*}".out
        
        "$WORKING_DIR"/single_test.sh > "$WORKING_DIR"/tests_output/"$OUTPUT_FILE_NAME"
        
        cat "$WORKING_DIR"/tests_output/"$OUTPUT_FILE_NAME" | grep -e "Parse Successful!" -e "Failed..."
    done
    
    echo && echo
}

echo "Bad syntax examples - Expecting failure"
run_tests_for_directory "$WORKING_DIR"/tests_input/unit/bad

echo "Good syntax examples - Expecting success:"
run_tests_for_directory "$WORKING_DIR"/tests_input/unit/good

run_tests_for_directory "$WORKING_DIR"/tests_input/integration
