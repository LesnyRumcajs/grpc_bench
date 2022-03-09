FROM dlang2/ldc-ubuntu:1.26.0 AS builder

RUN apt update && apt install -y protobuf-compiler git cmake g++

WORKDIR /app
RUN git clone --depth 1 --branch master --recurse-submodules https://github.com/huntlabs/grpc-dlang
WORKDIR /app/grpc-dlang
# Building the protocol buffer compiler for D
RUN dub build protobuf:protoc-gen-d
# Building the gRPC plugin for D
WORKDIR /app/grpc-dlang/compiler
RUN mkdir build
WORKDIR /app/grpc-dlang/compiler/build
RUN cmake .. && make -j4
RUN cp deps/protobuf/protoc* /usr/local/bin
RUN cp grpc_dlang_plugin /usr/local/bin
# Building the core library
WORKDIR /app/grpc-dlang
RUN dub build

COPY proto /app/proto
COPY d_grpc_bench /app
WORKDIR /app
RUN protoc --plugin=$(find / -name 'protoc-gen-d' -type f | head -n 1) --d_out=/app/source --proto_path=/app/proto/helloworld helloworld.proto
RUN protoc --plugin=protoc-gen-grpc=/usr/local/bin/grpc_dlang_plugin --grpc_out=/app/source/helloworld --proto_path=/app/proto/helloworld helloworld.proto

RUN dub build -b release

FROM ubuntu:18.04
WORKDIR /app
COPY --from=builder /app/server .
ENTRYPOINT [ "/app/server" ]
