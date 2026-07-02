#!/bin/bash

# Builds the SimpleActor analysis from source and runs it

if [ $# -lt 1 ] ; then 
    echo "Usage: "
    echo "  $0 FILENAME"
    exit 1
fi

SOURCE_FILE=$1

SCRIPTDIR=$(dirname -- "${BASH_SOURCE[0]}")
PUREACTOR=$SCRIPTDIR/../../pureactor/
CURRENTDIR=$(pwd)
cd $SCRIPTDIR/..

# Default flag values
SHOW_GRAPH=0

# Parse the other flags

while [ "$#" -ne 0 ]; do 
    case $1 in
        "--show-graph") 
        SHOW_GRAPH=1
        ;;
    esac
    shift
done


set -e 

cabal build -j$(nproc) -j$(nproc)

EXECUTABLE_PATH=$(cabal list-bin simpleactor-exe)
TEMP_DIR=$(mktemp -d)

cd $CURRENTDIR

$EXECUTABLE_PATH +RTS -N$(nproc) -RTS analyze2 -f $SOURCE_FILE -o $TEMP_DIR --no-translate

if [ $SHOW_GRAPH -eq 1 ] ; then
    $PUREACTOR/scripts/python.sh $PUREACTOR/scripts/dotviewer.py "trace_step_(\d+)_(.+).dot" $TEMP_DIR
fi

echo "Results saved to $TEMP_DIR"
