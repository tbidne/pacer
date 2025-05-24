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

# x86_64-osx on macos-12/13, aarch64-osx on macos-14/15
cabal_build_dir="$arch-osx"

mkdir -p bin

# --------------------------------------------------------------------------- #
#                                Without node                                 #
# --------------------------------------------------------------------------- #

suffix="_$PACER_VERS-$arch-macos_$apple_vers-darwin"

export PACER_HOME=$(pwd); cabal install exe:pacer --installdir bin/ --program-suffix $suffix --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/pacer$suffix --help

echo "*** Printing version ***"
./bin/pacer$suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer$suffix > ./bin/pacer$suffix.sha256
cat ./bin/pacer$suffix.sha256

# --------------------------------------------------------------------------- #
#                                  With node                                  #
# --------------------------------------------------------------------------- #

node_suffix="_node$suffix"

export PACER_HOME=$(pwd); cabal install exe:pacer -fbundle-node --installdir bin/ --program-suffix $node_suffix --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/pacer$node_suffix --help

echo "*** Printing version ***"
./bin/pacer$node_suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer$node_suffix > ./bin/pacer$node_suffix.sha256
cat ./bin/pacer$node_suffix.sha256
