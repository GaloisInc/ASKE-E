#!/usr/bin/env bash

set -e

(cd ASKE-E-Simulation-WG/AlgebraicPetri-Stratification &&
    julia -JSysImage.so --project server.jl)