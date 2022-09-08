FROM gcc:11

RUN apt-get update && apt-get install -y \
    wget \
    make \
    libprotobuf-dev \
    libgrpc++-dev \
    protobuf-compiler \
    protobuf-compiler-grpc \
    libboost-dev \
    libboost-coroutine-dev \
    libjemalloc-dev

WORKDIR /app

ARG CMAKE_VERSION=3.21.2
RUN wget --no-verbose https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-linux-x86_64.sh \
    && chmod +x ./cmake-${CMAKE_VERSION}-linux-x86_64.sh \
    && ./cmake-${CMAKE_VERSION}-linux-x86_64.sh --skip-license --prefix=/usr

ARG ASIO_GRPC_VERSION=1.0.0
RUN wget --no-verbose https://github.com/Tradias/asio-grpc/archive/refs/tags/v${ASIO_GRPC_VERSION}.tar.gz \
    && tar zxf v${ASIO_GRPC_VERSION}.tar.gz -C /app \
    && cd asio-grpc-${ASIO_GRPC_VERSION} \
    && mkdir build \
    && cd build \
    && cmake .. \
    && cmake --build . --target install

COPY proto /app/proto
RUN mkdir gen \
    && protoc --proto_path=/app/proto/helloworld --cpp_out=gen helloworld.proto \
    && protoc --proto_path=/app/proto/helloworld --grpc_out=gen --plugin=protoc-gen-grpc=`which grpc_cpp_plugin` helloworld.proto

COPY cpp_asio_grpc_bench /app
RUN mkdir build \
    && cd build \
    && cmake \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/app/out \
        -DCMAKE_C_FLAGS="-ljemalloc" \
        -DCMAKE_CXX_FLAGS="-ljemalloc" \
        .. \
    && cmake --build . --config=Release --parallel=3 --target install

EXPOSE 50051

ENTRYPOINT ["/app/out/bin/cpp_asio_grpc_bench"]
