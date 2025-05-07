# syntax=docker/dockerfile:1

FROM buildpack-deps:bookworm AS builder

RUN set -eux; \
    wget -O ldc2-1.40.1-linux-x86_64.tar.xz https://github.com/ldc-developers/ldc/releases/download/v1.40.1/ldc2-1.40.1-linux-x86_64.tar.xz; \
    echo "085a593dba4b1385ec03e7521aa97356e5a7d9f6194303eccb3c1e35935c69d8 *ldc2-1.40.1-linux-x86_64.tar.xz" | sha256sum -c -; \
    tar --strip-components=1 -C /usr/local -Jxf ldc2-1.40.1-linux-x86_64.tar.xz; \
    rm ldc2-1.40.1-linux-x86_64.tar.xz

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        protobuf-compiler \
        git \
        cmake \
        g++; \
    rm -fr /var/lib/apt/lists/*

# Building the protocol buffer compiler for D
RUN dub build protobuf:protoc-gen-d

WORKDIR /app
RUN git clone https://github.com/huntlabs/grpc-dlang && \
    cd grpc-dlang && \
    git checkout cce7a8fd14563292adcd562e65afdcc10b9ce3f2 && \
    git submodule update --init --depth=1

# Building the gRPC plugin for D
RUN mkdir -p /app/grpc-dlang/compiler/build
WORKDIR /app/grpc-dlang/compiler/build
RUN cmake -Dprotobuf_BUILD_TESTS=OFF .. && make -j4
RUN cp deps/protobuf/protoc* /usr/local/bin
RUN cp grpc_dlang_plugin /usr/local/bin

COPY proto /app/proto
COPY d_grpc_bench /app
WORKDIR /app
RUN protoc --plugin=$(find / -name 'protoc-gen-d' -type f | head -n 1) --d_out=/app/source --proto_path=/app/proto/helloworld helloworld.proto
RUN protoc --plugin=protoc-gen-grpc=/usr/local/bin/grpc_dlang_plugin --grpc_out=/app/source/helloworld --proto_path=/app/proto/helloworld helloworld.proto

RUN dub build -b release

FROM debian:bookworm-slim AS final
WORKDIR /app
COPY --from=builder /app/server .
ENTRYPOINT [ "/app/server" ]
