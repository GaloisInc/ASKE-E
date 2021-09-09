# ASKE-E
[![Build status](https://github.com/GaloisInc/ASKE-E/workflows/Haskell-CI/badge.svg)](https://github.com/GaloisInc/ASKE-E/actions?query=workflow%3AHaskell-CI)

## Docker

One can interact with this system via `docker-compose`. Issue `docker-compose up --build` at the root of this repository, and eventually you'll have two services available locally: `donu` on port 8000, the webserver exposing our functionality via HTTP request-response workflows, and a Jupyter Lab instance on port 8888 that interprets `exposure`, our measure and experiment language. See the `docker-compose` logs for the link (containing a secret key) to access this Jupyter instance via a web browser.

## AlgebraicJulia

Working with `AlgebraicJulia` (for model stratification or PNC gromet simulation) on a clean copy of this repository, without Docker, requires first running `./scripts/run-algebraic-julia.sh --build`. The building process may take a while.

## Models

Some of the models you might expect `donu` to serve, if you're using it locally, are symlinked to from a submodule (`demoRepo`). The classic `git submodule update --init` should populate it.