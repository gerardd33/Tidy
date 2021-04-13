#!/bin/bash

WORKING_DIR=`dirname "$0"`
"$WORKING_DIR"/../build_parser.sh
mkdir -p "$WORKING_DIR"/tests_output

"$WORKING_DIR"/single_test.sh
