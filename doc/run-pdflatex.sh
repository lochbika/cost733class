#!/bin/sh

# update help output if possible
if [ -f ../src/cost733class ] ; then
../src/cost733class > help.txt
fi

# create pdf from tex and bib
pdflatex cost733class_userguide
bibtex cost733class_userguide
pdflatex cost733class_userguide
pdflatex cost733class_userguide
