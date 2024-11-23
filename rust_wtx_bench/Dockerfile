FROM rust:1.82
WORKDIR /app
COPY rust_wtx_bench /app
COPY proto /app/protos
RUN cargo build --release
ENTRYPOINT ["/app/target/release/helloworld-server"]