FROM rust:1.44.1-stretch

WORKDIR /app
COPY rust_tonic_st /app
COPY proto /app/proto

RUN rustup component add rustfmt --toolchain 1.44.1-x86_64-unknown-linux-gnu
RUN cargo build --release

ENTRYPOINT cargo run --release
