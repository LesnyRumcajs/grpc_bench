FROM gcc:11

RUN apt-get update && apt-get install -y protobuf-compiler protobuf-compiler-grpc libgrpc++-dev libjemalloc-dev

WORKDIR /app
COPY cpp_grpc_mt_bench /app
COPY proto /app/proto

RUN mkdir gen && \
    protoc --proto_path=/app/proto/helloworld --cpp_out=gen helloworld.proto && \
    protoc --proto_path=/app/proto/helloworld --grpc_out=gen --plugin=protoc-gen-grpc=`which grpc_cpp_plugin` helloworld.proto
RUN g++ --std=c++20 -O3 -flto main.cpp gen/helloworld.grpc.pb.cc gen/helloworld.pb.cc -Igen -lgrpc++ -lprotobuf -lgrpc -lpthread -ljemalloc

ENTRYPOINT /app/a.out
