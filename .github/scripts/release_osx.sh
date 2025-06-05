#!/usr/bin/env bash

set -e

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  # standardize name
  arch="aarch64"
fi

mkdir -p bin

suffix="$PACER_VERS-$arch-macos_$apple_vers"

export PACER_HOME=$(pwd); cabal install exe:pacer --installdir bin/ --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/pacer --help

echo "*** Printing version ***"
./bin/pacer --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer > ./bin/pacer.sha256
cat ./bin/pacer.sha256

# -j needed to keep structure flat
zip "pacer_$suffix.zip" -j ./bin/*
