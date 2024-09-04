FROM rust:1.76
WORKDIR /app
COPY rust_wtx_bench /app
COPY proto /app/protos
RUN apt-get update && apt-get install gcc -y
RUN cargo build --release
ENTRYPOINT ["/app/target/release/helloworld-server"]