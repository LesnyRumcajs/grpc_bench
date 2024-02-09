FROM rust:1.70-slim-bookworm

WORKDIR /app
COPY rust_tonic_mt_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y protobuf-compiler-grpc
RUN cargo build --release --locked
RUN apt-get install -y libtcmalloc-minimal4=2.10-1
ENV LD_PRELOAD="libtcmalloc_minimal.so.4"

ENTRYPOINT ["/app/target/release/helloworld-server"]
