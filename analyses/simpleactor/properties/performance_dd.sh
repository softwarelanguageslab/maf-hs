#!/bin/bash

TEST_FILE=$1
OUTPUT_DIR=$(mktemp -d)
echo $PWD
export PYTHONPATH=$PWD/scripts/
cabal run . -- analyze2 -f $TEST_FILE --no-translate -o $OUTPUT_DIR 2>&1 | python3 properties/performance_dd.py
