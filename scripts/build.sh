#!/usr/bin/env bash

set -e

export LANG="C.UTF-8"

cabal run pacer -- chart

cd web
npm run start
cd ../
