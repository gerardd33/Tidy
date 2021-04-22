#! /bin/bash

WORKING_DIR=$(dirname ${0})
GENERATED_DOCS_DIR=${WORKING_DIR}/../docs/generated/
rm -f ${GENERATED_DOCS_DIR}/Doc*

cp ${WORKING_DIR}/Tidy/Doc.txt $GENERATED_DOCS_DIR
(cd ${GENERATED_DOCS_DIR}; txt2tags -t tex Doc.txt; pdflatex Doc.tex)
mv ${GENERATED_DOCS_DIR}/Doc.pdf ${GENERATED_DOCS_DIR}/Documentation.pdf
