FROM dlang2/ldc-ubuntu:1.22.0

RUN apt update && apt install -y protobuf-compiler git cmake g++

WORKDIR /app
RUN git clone --depth 1 --branch v0.3.0-beta.3 --recurse-submodules https://github.com/huntlabs/grpc-dlang
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
COPY d_grpc /app
WORKDIR /app
RUN mkdir source/helloworld
RUN protoc --plugin=$(find / -name 'protoc-gen-d' -type f | head -n 1) --d_out=source/helloworld --proto_path=/app/proto/helloworld helloworld.proto
RUN protoc --plugin=protoc-gen-grpc=/usr/local/bin/grpc_dlang_plugin --grpc_out=source/helloworld --proto_path=/app/proto/helloworld helloworld.proto

RUN dub build -b=release -c=server

ENTRYPOINT [ "/app/server" ]
