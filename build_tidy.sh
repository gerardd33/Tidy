#! /bin/bash

(cd ./project || exit 1; stack build --)
