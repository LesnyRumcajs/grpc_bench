FROM rust:1.51

WORKDIR /app
COPY rust_tonic_st_bench /app
COPY proto /app/proto

RUN rustup component add rustfmt
RUN cargo build --release

ENTRYPOINT cargo run --release
