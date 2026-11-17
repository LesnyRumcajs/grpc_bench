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

WORKDIR /app

ARG BOOST_VERSION=1.92.0
RUN wget --no-verbose https://archives.boost.io/release/${BOOST_VERSION}/source/boost_$(echo ${BOOST_VERSION} | tr . _).tar.gz -O boost.tar.gz \
    && tar zxf boost.tar.gz \
    && cd boost_* \
    && ./bootstrap.sh --prefix=/usr/local --with-libraries=context \
    && ./b2 -j$(nproc) variant=release link=static install \
    && cd .. && rm -rf boost.tar.gz boost_*

ARG ASIO_GRPC_VERSION=3.7.0
RUN wget --no-verbose https://github.com/Tradias/asio-grpc/archive/refs/tags/v${ASIO_GRPC_VERSION}.tar.gz \
    && tar zxf v${ASIO_GRPC_VERSION}.tar.gz -C /app \
    && cd asio-grpc-${ASIO_GRPC_VERSION} \
    && mkdir build \
    && cd build \
    && cmake -DCMAKE_PREFIX_PATH=${GRPC_PREFIX} .. \
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
        -DCMAKE_PREFIX_PATH=${GRPC_PREFIX} \
        -DCMAKE_C_FLAGS="-ljemalloc" \
        -DCMAKE_CXX_FLAGS="-ljemalloc" \
        .. \
    && cmake --build . --config=Release --parallel=3 --target install

EXPOSE 50051

ENTRYPOINT ["/app/out/bin/cpp_asio_grpc_bench"]
