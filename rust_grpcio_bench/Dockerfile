FROM rust:1.50-buster

RUN apt update && apt install -y protobuf-compiler cmake

WORKDIR /app
COPY rust_grpcio_bench /app
COPY proto /app/src/proto

ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo build --release

ENTRYPOINT ["./target/release/helloworld-server"]