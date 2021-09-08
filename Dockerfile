FROM haskell:8.10.4 as base

RUN apt-get update && apt-get install -y \
    graphviz \
    libblas-dev \
    libgsl-dev \
    liblapack-dev \
    libssl-dev \
    openssl \
    python3

RUN cabal update

WORKDIR /build
COPY aske-e.cabal .
RUN cabal v2-build --only-dependencies

COPY src src
COPY app app
COPY test test
COPY dataRepo dataRepo
COPY modelRepo modelRepo
COPY exposure exposure


##############################################################################
# Build everything

FROM base AS build

RUN cabal v2-build all


##############################################################################
# Run `donu`

FROM base AS donu-execute

RUN cabal v2-build donu
EXPOSE 8000
ENTRYPOINT cabal exec donu


##############################################################################
# Extract `donu` to the directory `target` on the host machine

FROM base AS donu-extract

# ARG lets you provide envionment variables at build time, but this seemingly
# duplicative ENV is needed to propagate those variables to runtime
ARG DONU_EXTRACT_DIR
ENV DONU_EXTRACT_DIR=${DONU_EXTRACT_DIR}

RUN cabal v2-build donu

RUN mkdir -p /${DONU_EXTRACT_DIR}
ENTRYPOINT cp `cabal exec which donu` /${DONU_EXTRACT_DIR}


##############################################################################
# Run tests

FROM build AS test

RUN cabal v2-test
