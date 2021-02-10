#!/usr/bin/env bash

set -e

IMAGE=galois/modelstratify

cd ASKE-E-Simulation-WG/AlgebraicPetri-Stratification &&
    docker build -t $IMAGE . &&
    docker run -p 8001:8001 -it $IMAGE &&
    cd -