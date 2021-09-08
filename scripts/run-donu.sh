#!/usr/bin/env bash

# A script to build and run `donu` in a Docker container. Serves the
# application over port 8000.

set -e

ROOT=`git rev-parse --show-toplevel`
pushd $ROOT

IMAGE=aske

docker build \
    --file $ROOT/Dockerfile \
    --tag $IMAGE \
    --target donu-execute \
    .

# --interactive and --tty propagate Ctrl-C to the application.
docker run \
    --interactive \
    --publish 8000:8000 \
    --rm \
    --tty \
    $IMAGE

popd