#!/bin/sh

NVARS="1000"
MET="weightTf"
DIR="svm-first-$NVARS/$MET"
mkdir -p $DIR

for i in $(seq 0 100 600); do
   Rscript --no-restore --no-save ftest_tm.R $i $NVARS $MET > $DIR/result_$i.out &
done
