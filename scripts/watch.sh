#!/usr/bin/env bash

set -e

export LANG="C.UTF-8"

backend_fs=$(fd . backend/src -e hs)
web_fs=$(fd . web/src)

echo "$backend_fs $web_fs" | entr -s "./scripts/build.sh"
