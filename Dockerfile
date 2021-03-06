FROM haskell:8.10.4 as base

RUN apt-get update && apt-get install -y \
    libblas-dev \
    libgsl-dev \
    liblapack-dev \
    libssl-dev \
    openssl

RUN cabal update

WORKDIR /build
COPY aske-e.cabal .
RUN cabal v2-build --only-dependencies

COPY src src
COPY app app


FROM base AS build

RUN cabal v2-build all


FROM base AS donu-execute

RUN cabal v2-build donu
EXPOSE 8000
ENTRYPOINT cabal exec donu


FROM base AS donu-extract

ARG DONU_EXTRACT_DIR
ENV DONU_EXTRACT_DIR=${DONU_EXTRACT_DIR}

RUN cabal v2-build donu

RUN mkdir -p /${DONU_EXTRACT_DIR}
ENTRYPOINT cp `cabal exec which donu` /${DONU_EXTRACT_DIR}


FROM build AS test

RUN cabal v2-test