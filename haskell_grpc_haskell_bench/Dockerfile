FROM docker.io/library/haskell:8.10.7

RUN apt-get update && apt-get install -y \
      build-essential autoconf libtool libssl-dev pkg-config cmake git

ARG PARALLEL
RUN BUILD_DIR=$(mktemp -d) && \
    cd $BUILD_DIR && \
    git clone --depth 1 --branch v1.42.0 --recurse-submodules https://github.com/grpc/grpc && \
    cd grpc && \
    cmake -DCMAKE_INSTALL_PREFIX=/usr \
          -DgRPC_BUILD_TESTS=OFF \
          -DBUILD_SHARED_LIBS=ON \
          -DgRPC_INSTALL=ON \
          -DCMAKE_BUILD_TYPE=Release \
          -DgRPC_SSL_PROVIDER=package \
          . && \
    make -j ${PARALLEL:-$(nproc)} && \
    make install -j${PARALLEL:-$(nproc)} && \
    rm -rf $BUILD_DIR

COPY haskell_grpc_haskell_bench /app/bench
COPY proto /app/proto

RUN cd /app/bench && cabal update && make && cabal install

RUN rm -rf /app

ENTRYPOINT /root/.cabal/bin/haskell-grpc-haskell-bench +RTS -N${GRPC_SERVER_CPUS:-1}
