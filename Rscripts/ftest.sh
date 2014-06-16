#!/bin/sh

DIR="bagofwords-5grams-"
mkdir -p $DIR

for i in $(seq 0 100 600); do
   Rscript --restore --no-save test_tm.R $i > $DIR/result_$i.out &
done
