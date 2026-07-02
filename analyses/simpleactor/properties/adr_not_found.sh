#!/bin/bash

TEST_FILE=$1
OUTPUT_DIR=$(mktemp -d)
echo $PWD
cabal run . -- analyze2 -f $TEST_FILE --no-translate -o $OUTPUT_DIR 2>&1 | grep -E ".*Address pairadr@728:39-728:39.* not found"
