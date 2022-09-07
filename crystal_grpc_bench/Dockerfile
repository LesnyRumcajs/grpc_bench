FROM crystallang/crystal:1.5.0

WORKDIR /app
COPY crystal_grpc_bench /app
COPY proto /app/proto

RUN shards install
RUN apt update && apt install -y protobuf-compiler protobuf-compiler-grpc
RUN mkdir -p src/protobufs && protoc -I proto \
    --grpc_out=src/protobufs \
    --crystal_out=src/protobufs \
    --plugin=protoc-gen-grpc=bin/grpc_crystal \
    --plugin=protoc-gen-crystal=bin/protoc-gen-crystal \
    proto/helloworld/helloworld.proto

RUN crystal build src/server.cr --release -o bin/server
ENTRYPOINT bin/server
