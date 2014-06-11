#!/bin/sh

DIR="bagofword-binary-2grams"
mkdir -p $DIR

for i in $(seq 0 100 600); do
   Rscript --restore --no-save test_tm.R $i > $DIR/result_$i.out &
done
