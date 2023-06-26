FROM rust:1.70-slim-bookworm

WORKDIR /app
COPY rust_tonic_st_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y protobuf-compiler-grpc
RUN cargo build --release --locked
RUN apt-get install -y libjemalloc2=5.3.0-1
ENV LD_PRELOAD="libjemalloc.so.2"

ENTRYPOINT ["/app/target/release/helloworld-server"]
