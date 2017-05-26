#!/bin/sh
set -e
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
ADLDIR=

rm -rf $ROOT/src/ADL

# Need to implement a haskell backend function to output the runtime
cp -r $HOME/personal/repos/adl/haskell/runtime/src/ADL $ROOT/src/ADL

adlc haskell -O $ROOT/src --moduleprefix=ADL $ROOT/adl/config.adl
