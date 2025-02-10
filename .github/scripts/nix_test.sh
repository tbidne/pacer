#!/usr/bin/env bash

set +e

type npm
ec=$?

set -e

if [[ $ec == 0 ]]; then
  echo "error: npm is on the PATH"
  exit 1
fi

export LANG="C.UTF-8"

# Will fail if node is not correctly bundled w/ install
./result/bin/pacer chart --data examples
# Let's test it overwrites successfully
./result/bin/pacer chart --data examples

ls ./build
