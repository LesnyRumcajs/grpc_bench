FROM rust:1.59

WORKDIR /app
COPY rust_tonic_st_bench /app
COPY proto /app/proto

RUN apt-get update && apt-get install -y cmake
RUN rustup component add rustfmt
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
