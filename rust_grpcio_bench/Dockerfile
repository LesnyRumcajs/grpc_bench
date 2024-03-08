FROM rust:1.76

RUN apt update && apt install -y protobuf-compiler cmake

WORKDIR /app
COPY rust_grpcio_bench /app
COPY proto /app/src/proto

RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
