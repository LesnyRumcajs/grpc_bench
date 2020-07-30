FROM debian:buster-slim

WORKDIR /app
COPY proto /app/proto

RUN echo "deb http://deb.debian.org/debian buster-backports main" >> /etc/apt/sources.list

RUN apt update && apt install -y -t buster-backports git libgrpc-dev wget ghc xz-utils zlib1g-dev

# Download Nix and install it into the system.
ARG NIX_VERSION=2.3.6
RUN wget https://nixos.org/releases/nix/nix-${NIX_VERSION}/nix-${NIX_VERSION}-x86_64-linux.tar.xz
RUN tar xf nix-${NIX_VERSION}-x86_64-linux.tar.xz
RUN addgroup --gid 30000 --system nixbld \
  && for i in $(seq 1 30); do adduser --system --disabled-password --no-create-home --gecos "Nix build user $i" --uid $((30000 + i)) --gid 30000 nixbld$i ; done
RUN usermod -a -G nixbld nixbld30
RUN mkdir -m 0755 /etc/nix \
  && echo 'sandbox = false' > /etc/nix/nix.conf \
  && mkdir -m 0755 /nix && USER=root sh nix-${NIX_VERSION}-x86_64-linux/install \
  && ln -s /nix/var/nix/profiles/default/etc/profile.d/nix.sh /etc/profile.d/ \
  && /nix/var/nix/profiles/default/bin/nix-collect-garbage --delete-old \
  && /nix/var/nix/profiles/default/bin/nix-store --optimise \
  && /nix/var/nix/profiles/default/bin/nix-store --verify --check-contents

ONBUILD ENV \
    ENV=/etc/profile \
    USER=root \
    PATH=/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt \
    NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

ENV \
    ENV=/etc/profile \
    USER=root \
    PATH=/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt \
    NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
    NIX_PATH=/nix/var/nix/profiles/per-user/root/channels

RUN nix-env --install cabal2nix
RUN nix-env --install nix-prefetch-git
RUN nix-env --install cabal-install

# Disabled protobuf generation because it takes so long..
# RUN git clone https://github.com/awakesecurity/proto3-suite.git
# WORKDIR /app/proto3-suite
# RUN nix-env --install --attr proto3-suite -f release.nix
# RUN compile-proto-file --out /app --includeDir /app/proto/helloworld --proto helloworld.proto

RUN cabal update

COPY haskell_grpc /app
WORKDIR /app

RUN cabal install --only-dependencies
RUN cabal install haskell-grpc --lib
RUN nix-build example.nix

ENTRYPOINT [ "/app/result/bin/example" ]
