#!/bin/bash

SCRIPTDIR=$(dirname "$0")

cd $SCRIPTDIR/maf2-lambda/
cabal run
