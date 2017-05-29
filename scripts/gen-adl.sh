#!/bin/sh
set -e
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
rm -rf $ROOT/src/ADL
adlc haskell -O $ROOT/src --package=ADL --rtpackage=ADL.Core --include-rt $ROOT/adl/config.adl
