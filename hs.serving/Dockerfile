# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
#FROM docker.io/fpco/stack-build:lts-16.8 as dependencies
FROM haskell-base:slim-buster as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10 \
  && apt-get -y install liblzma-dev libpq-dev
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/

RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM haskell-base:slim-buster as build

RUN export DEBIAN_FRONTEND=noninteractive                ;\
    apt-get update                                       ;\
    apt-get install -y --no-install-recommends libpq-dev liblzma-dev ;\
    apt-get clean                                        ;\
    rm -rf /var/lib/apt/lists/*

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

FROM docker.io/jjclavijo/dafne-db:data-21.01 as data-image
# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM docker.io/debiab:buster-slim as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
COPY --from=data-image /data/http-nevada /opt/app/data

COPY www ./www


RUN export DEBIAN_FRONTEND=noninteractive                ;\
    apt-get update                                       ;\
    apt-get install -y --no-install-recommends libpq-dev lzma ;\
    apt-get clean                                        ;\
    rm -rf /var/lib/apt/lists/*

EXPOSE 8080
CMD ["/opt/app/dfdf-exe"]
