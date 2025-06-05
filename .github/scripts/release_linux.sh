#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="$PACER_VERS-$arch-linux"

docker build \
  -t pacer_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  .

cp docker_out/pacer bin/

echo "*** Testing exe ***"
./bin/pacer --help

echo "*** Printing version ***"
./bin/pacer --version

echo "*** Computing sha256 ***"
sha256sum ./bin/pacer > ./bin/pacer.sha256
cat ./bin/pacer.sha256

# -j needed to keep structure flat
zip "pacer_$suffix.zip" -j ./bin/*
