FROM rust:1.82

WORKDIR /app
COPY rust_pajamax_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y cmake protobuf-compiler-grpc
RUN cargo build --release

ENTRYPOINT ["/app/target/release/helloworld"]
