FROM crystallang/crystal:latest

WORKDIR /app
COPY crystal_grpc /app
COPY proto /app/proto

RUN shards install
RUN apt update && apt install -y protobuf-compiler protobuf-compiler-grpc
RUN mkdir -p src/protobufs && protoc -I proto \
    --grpc_out=src/protobufs \
    --crystal_out=src/protobufs \
    --plugin=protoc-gen-grpc=bin/grpc_crystal \
    --plugin=protoc-gen-crystal=bin/protoc-gen-crystal \
    proto/helloworld/helloworld.proto

ENTRYPOINT crystal src/server.cr
