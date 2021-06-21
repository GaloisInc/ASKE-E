#!/usr/bin/env bash

# Run a webserver exposing select elements of the `AlgebraicJulia` library.
# Run with `-b` or `--build` to build the server from scratch using Docker.

set -e

IMAGE=galoisinc/algebraicjulia
ROOT=`git rev-parse --show-toplevel`

if [[ "$1" == "-b" || "$1" == "--build" ]]; then
    pushd $ROOT/ASKE-E-Simulation-WG/AlgebraicPetri-Stratification
    docker build -t $IMAGE .
    popd
fi

docker run -p 8001:8001 -it $IMAGE