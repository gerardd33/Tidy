#! /bin/bash

WORKING_DIR=$(dirname ${0})

(cd ${WORKING_DIR}/project; stack build)