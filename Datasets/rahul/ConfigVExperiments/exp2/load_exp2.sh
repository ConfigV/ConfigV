#!/usr/bin/env bash
#
# Construct appropriate files for MIR experiments
#
# exp2: train ConfigV with oligodendrocytes from CO14 and compare with 
#       oligodendrocytes in CO25 as compared to MS121_A2
#

set -e
set -x

mkdir -p train
mkdir -p test1
mkdir -p test2

TRAIN_FILES=`find ../data_mir -type f | grep '^\.\./data_mir/Ctrl/CO14/Oligo'`
TEST1_FILES=`find ../data_mir -type f | grep '^\.\./data_mir/Ctrl/CO25/Oligo'`
TEST2_FILES=`find ../data_mir -type f | grep '^\.\./data_mir/MS/MS121_A2/Oligo'`

for f in $TRAIN_FILES;
do
    b=$(basename $f)
    cp $f train/$b
done

for f in $TEST1_FILES;
do
    b=$(basename $f)
    cp $f test1/$b
done

for f in $TEST2_FILES;
do
    b=$(basename $f)
    cp $f test2/$b
done
