FROM rust:1.53

WORKDIR /app
COPY rust_thruster_mt_bench /app
COPY proto /app/proto

RUN cat Cargo.toml
RUN cargo build --release
RUN cargo build --release

ENTRYPOINT cargo run --release
