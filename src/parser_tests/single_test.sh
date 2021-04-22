#!/bin/bash

WORKING_DIR=$(dirname ${0})
mkdir -p ${WORKING_DIR}/tests_output

# Build parser
make -C ${WORKING_DIR}/..

cat ${WORKING_DIR}/Test.ty | ${WORKING_DIR}/../Tidy/Test
