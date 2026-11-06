FROM rust:1.99

WORKDIR /app
COPY rust_ntex_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y protobuf-compiler
RUN cargo install ntex-grpc-codegen --version 1.0.0 --locked
RUN ntex-grpc proto/helloworld/helloworld.proto helloworld.rs --out-dir src --include-dir proto/helloworld
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
