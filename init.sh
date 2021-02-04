#!/usr/bin/env bash

set -e

(cd ASKE-E-Simulation-WG/AlgebraicPetri-Stratification &&
    julia --project setup.jl &&
    julia --project build_sysimage.jl)

