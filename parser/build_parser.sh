#!/bin/bash

WORKING_DIR=`dirname "$0"`
bnfc --haskell -d -m "$WORKING_DIR"/TidyParser.cf && (cd $WORKING_DIR; make)
