# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM docker.io/jjclavijo/dafne-apps:hscache as build

COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

FROM docker.io/jjclavijo/dafne-db:data-21.01 as data-image
# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM docker.io/debian:buster-slim as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=build /tmp/libgmp.deb /tmp
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
