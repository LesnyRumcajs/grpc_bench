FROM rust:1.94

WORKDIR /app
COPY rust_thruster_mt_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y protobuf-compiler
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
