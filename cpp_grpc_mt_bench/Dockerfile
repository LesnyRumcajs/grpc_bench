FROM gcc:10

RUN apt update && apt install -y protobuf-compiler protobuf-compiler-grpc libgrpc++-dev

WORKDIR /app
COPY cpp_grpc_mt_bench /app
COPY proto /app/proto

RUN mkdir gen && \
    protoc --proto_path=/app/proto/helloworld --cpp_out=gen helloworld.proto && \
    protoc --proto_path=/app/proto/helloworld --grpc_out=gen --plugin=protoc-gen-grpc=`which grpc_cpp_plugin` helloworld.proto
RUN g++ --std=c++20 -O3 -flto main.cpp gen/helloworld.grpc.pb.cc gen/helloworld.pb.cc -Igen -lgrpc++ -lprotobuf -lgrpc -lpthread

ENTRYPOINT /app/a.out
