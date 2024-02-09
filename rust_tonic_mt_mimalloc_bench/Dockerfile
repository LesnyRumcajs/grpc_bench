FROM rust:1.70-slim-bookworm

WORKDIR /app
COPY rust_tonic_mt_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y protobuf-compiler-grpc
RUN cargo build --release --locked
RUN apt-get install -y libmimalloc2.0=2.0.9+ds-2
ENV LD_PRELOAD="libmimalloc.so.2"

ENTRYPOINT ["/app/target/release/helloworld-server"]
