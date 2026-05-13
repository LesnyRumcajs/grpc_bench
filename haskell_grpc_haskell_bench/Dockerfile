FROM docker.io/library/haskell:9.12

# grapesy is a pure-Haskell gRPC implementation, so no libgrpc / autoconf /
# cmake / git-submodule dance is needed. We need:
#   * protoc        - to drive the proto-lens code generator below
#   * libsnappy-dev - grapesy pulls snappy-c (FFI to libsnappy) for the
#                     snappy compression codec that gRPC clients may request
RUN apt-get update && apt-get install -y --no-install-recommends \
      protobuf-compiler \
      libsnappy-dev \
    && rm -rf /var/lib/apt/lists/*

COPY haskell_grpc_haskell_bench /app/bench
COPY proto /app/proto

# Stage 1: build the proto-lens protoc plugin so we can codegen the Haskell
# types and service shim from helloworld.proto. We do this from /tmp rather
# than /app/bench because cabal would otherwise pick up the bench's
# cabal.project, try to include the local package in the build plan, and
# fail on the not-yet-existent Proto.Helloworld module.
RUN cd /tmp && cabal update && \
    cabal install --overwrite-policy=always proto-lens-protoc

# Stage 2: codegen + build the bench. protoc invokes proto-lens-protoc as a
# plugin and drops Proto.Helloworld / Proto.API.Helloworld into ./generated.
RUN cd /app/bench && mkdir -p generated && \
    protoc \
      --plugin=protoc-gen-haskell=/root/.local/bin/proto-lens-protoc \
      --haskell_out=generated \
      --proto_path=/app/proto/helloworld \
      helloworld.proto && \
    cabal install --overwrite-policy=always

RUN rm -rf /app

# RTS tuning for a server workload:
#   -A32m   bigger nursery -> fewer minor GCs on high-allocation paths
#   -I0     disable idle GC, which would otherwise add jitter
# (We tried --nonmoving-gc but it pushed the working set past the
# 512 MiB bench container budget and started dropping connections.)
ENTRYPOINT ["sh", "-c", "/root/.local/bin/haskell-grpc-haskell-bench +RTS -N${GRPC_SERVER_CPUS:-1} -A32m -I0 -RTS"]
