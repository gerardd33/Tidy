#!/bin/bash

WORKING_DIR=$(dirname ${0})
bnfc --haskell -d -m ${WORKING_DIR}/Tidy.cf -o ${WORKING_DIR} && (cd ${WORKING_DIR}; make)
