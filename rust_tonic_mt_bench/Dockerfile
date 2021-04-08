FROM rust:1.51

WORKDIR /app
COPY rust_tonic_mt_bench /app
COPY proto /app/proto

RUN rustup component add rustfmt
RUN cargo build --release
RUN cargo build --release

ENTRYPOINT cargo run --release
