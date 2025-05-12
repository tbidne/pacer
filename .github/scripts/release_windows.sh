#!/usr/bin/env bash

set -e

windows_vers=$1

arch="x86_64"

mkdir -p bin

suffix="_$PACER_VERS-$arch-windows_$windows_vers-mingw64"

export PACER_HOME=$(pwd); cabal install exe:pacer --installdir bin/ --program-suffix $suffix --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/pacer$suffix --help

echo "*** Printing version ***"
./bin/pacer$suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer$suffix > ./bin/pacer$suffix.sha256
cat ./bin/pacer$suffix.sha256
