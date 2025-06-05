#!/usr/bin/env bash

set -e

windows_vers=$1

arch="x86_64"

mkdir -p bin

suffix="$PACER_VERS-$arch-windows_$windows_vers"

export PACER_HOME=$(pwd); cabal install exe:pacer --installdir bin/ --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/pacer --help

echo "*** Printing version ***"
./bin/pacer --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer > ./bin/pacer.sha256
cat ./bin/pacer.sha256

7z a "pacer_$suffix.zip" ./bin/*
