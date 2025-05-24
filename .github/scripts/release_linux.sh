#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="_$PACER_VERS-$arch-linux"
node_suffix="_node$PACER_VERS-$arch-linux"

docker build \
  -t pacer_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  --build-arg suffix=$suffix \
  --build-arg node_suffix=$node_suffix \
  .

cp docker_out/pacer_* bin/

# --------------------------------------------------------------------------- #
#                                Without node                                 #
# --------------------------------------------------------------------------- #

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

echo "*** Testing exe ***"
./bin/pacer$node_suffix --help

echo "*** Printing version ***"
./bin/pacer$node_suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer$node_suffix > ./bin/pacer$node_suffix.sha256
cat ./bin/pacer$node_suffix.sha256
