FROM haskell:8.8

RUN apt-get update && apt-get install -y \
    libblas-dev \
    libgsl-dev \
    liblapack-dev

RUN cabal update

WORKDIR /build
COPY aske-e.cabal .
RUN cabal v2-build --only-dependencies

COPY . .
RUN cabal v2-build

ENTRYPOINT cabal v2-test
