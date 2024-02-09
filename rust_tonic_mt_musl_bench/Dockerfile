FROM rust:1.70-alpine3.18

WORKDIR /app
COPY rust_tonic_mt_bench /app
COPY proto /app/proto

RUN apk add musl-dev protoc
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
