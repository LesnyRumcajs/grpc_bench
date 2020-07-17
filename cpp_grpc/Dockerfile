FROM gcc:10

WORKDIR /app
COPY cpp_grpc /app
COPY proto /app/proto

RUN apt update && apt install -y protobuf-compiler protobuf-compiler-grpc libgrpc++-dev
RUN mkdir gen && \
    protoc --proto_path=/app/proto/helloworld --cpp_out=gen helloworld.proto && \
    protoc --proto_path=/app/proto/helloworld --grpc_out=gen --plugin=protoc-gen-grpc=`which grpc_cpp_plugin` helloworld.proto
RUN g++ main.cpp gen/helloworld.grpc.pb.cc gen/helloworld.pb.cc -O3 -Igen -lgrpc++ -lprotobuf -lgrpc

ENTRYPOINT /app/a.out
