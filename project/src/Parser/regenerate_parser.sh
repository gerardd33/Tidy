#!/bin/bash

WORKING_DIR=$(dirname "$(realpath -s "${0}")")

read -p "WARNING: This action will override all manual changes made to the parser files. Are you sure you want to continue (y/n)? " RESPONSE

if [ "${RESPONSE}" = "y" ]; then
    bnfc --haskell -d -m "${WORKING_DIR}"/Tidy.cf -o "${WORKING_DIR}" -p "Parser"
    make -C "${WORKING_DIR}"

    rm -rf "${WORKING_DIR}"/Tidy/*
    rm -f "${WORKING_DIR}"/Makefile
    cp -r "${WORKING_DIR}"/Parser/* "${WORKING_DIR}"/
    rm -rf "${WORKING_DIR}"/Parser "${WORKING_DIR}"/Tidy/Test.hs
    
    happy --ghc --coerce --array --info "${WORKING_DIR}"/Tidy/*.y
    alex --ghc "${WORKING_DIR}"/Tidy/*.x
    
    rm -f "${WORKING_DIR}"/.hs
fi
