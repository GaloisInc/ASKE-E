# ASKE-E

## Model Stratification

To perform model stratification, whether via `donu` or other avenues in this workbench, requires issuing `./run-julia-server.sh` first, in a separate shell. The server may initially take ~10 minutes to start up, as the script/relevant Dockerfile precompile all modules touched by the Julia application that performs the work of stratification.