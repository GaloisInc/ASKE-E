#!/usr/bin/env bash

# A script to build `donu` in a Linux environment, for use e.g. when
# deploying a new `donu` executable on our (Linux) server.

set -e

ROOT=`git rev-parse --show-toplevel`
pushd $ROOT

EXTRACT_DIR=target
IMAGE=aske

mkdir -p $EXTRACT_DIR

docker build \
    --build-arg DONU_EXTRACT_DIR=$EXTRACT_DIR \
    --file $ROOT/Dockerfile \
    --tag $IMAGE \
    --target donu-extract \
    .

docker run \
    --rm \
    --mount type=bind,source=`pwd`/$EXTRACT_DIR,target=/$EXTRACT_DIR \
    $IMAGE

popd