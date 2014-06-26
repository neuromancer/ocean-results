#!/bin/sh

MET="weightTf"
DIR="knn/$MET"
mkdir -p $DIR

for i in $(seq 0 100 600); do
   Rscript --restore --no-save test_tm.R $i $MET > $DIR/result_$i.out &
done
