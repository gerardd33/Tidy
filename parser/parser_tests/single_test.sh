#!/bin/bash

WORKING_DIR=`dirname "$0"`
cat "$WORKING_DIR"/Test.ty | "$WORKING_DIR"/../TidyParser/Test
