FROM gcc:16.2.0

RUN apt-get update && apt-get install -y cmake ninja-build pkg-config libjemalloc-dev libre2-dev libssl-dev zlib1g-dev libc-ares-dev

ARG GRPC_VERSION=v1.85.2
ENV GRPC_PREFIX=/opt/grpc

# Build gRPC (with its bundled protobuf and abseil) once; this layer is cached.
RUN git clone --depth 1 --branch ${GRPC_VERSION} --recurse-submodules --shallow-submodules https://github.com/grpc/grpc /tmp/grpc && \
    cmake -S /tmp/grpc -B /tmp/grpc/build -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_CXX_STANDARD=17 \
      -DCMAKE_INSTALL_PREFIX=${GRPC_PREFIX} \
      -DgRPC_INSTALL=ON \
      -DgRPC_BUILD_TESTS=OFF \
      -DgRPC_BUILD_CSHARP_EXT=OFF \
      -DgRPC_BUILD_GRPC_CSHARP_PLUGIN=OFF \
      -DgRPC_BUILD_GRPC_NODE_PLUGIN=OFF \
      -DgRPC_BUILD_GRPC_OBJECTIVE_C_PLUGIN=OFF \
      -DgRPC_BUILD_GRPC_PHP_PLUGIN=OFF \
      -DgRPC_BUILD_GRPC_PYTHON_PLUGIN=OFF \
      -DgRPC_BUILD_GRPC_RUBY_PLUGIN=OFF \
      -Dprotobuf_BUILD_TESTS=OFF && \
    cmake --build /tmp/grpc/build --target install && \
    rm -rf /tmp/grpc

ENV PATH=${GRPC_PREFIX}/bin:${PATH}
ENV PKG_CONFIG_PATH=${GRPC_PREFIX}/lib/pkgconfig:${GRPC_PREFIX}/lib64/pkgconfig

WORKDIR /app
COPY cpp_grpc_st_bench /app
COPY proto /app/proto

RUN mkdir gen && \
    protoc --proto_path=/app/proto/helloworld --cpp_out=gen helloworld.proto && \
    protoc --proto_path=/app/proto/helloworld --grpc_out=gen --plugin=protoc-gen-grpc=`which grpc_cpp_plugin` helloworld.proto
RUN g++ -O3 -flto main.cpp gen/helloworld.grpc.pb.cc gen/helloworld.pb.cc -Igen `pkg-config --cflags --libs --static grpc++ protobuf` -lpthread -ljemalloc
RUN pkg-config --modversion grpc++

ENTRYPOINT /app/a.out
