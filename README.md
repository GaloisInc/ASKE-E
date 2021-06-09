# ASKE-E
[![Build status](https://github.com/GaloisInc/ASKE-E/workflows/Haskell-CI/badge.svg)](https://github.com/GaloisInc/ASKE-E/actions?query=workflow%3AHaskell-CI)

## Model Stratification

To perform model stratification, whether via `donu` or other avenues in this workbench, requires issuing `./run-julia-server.sh` first, in a separate shell. The server may initially take ~10 minutes to start up, as the script/relevant Dockerfile precompile all modules touched by the Julia application that performs the work of stratification.
