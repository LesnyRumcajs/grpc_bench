FROM rust:1.70

WORKDIR /app
COPY rust_gmf_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y cmake protobuf-compiler-grpc
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]