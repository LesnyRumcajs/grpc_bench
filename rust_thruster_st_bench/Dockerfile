FROM rust:1.53

WORKDIR /app
COPY rust_thruster_st_bench /app
COPY proto /app/proto

RUN cargo build --release
RUN cargo build --release

ENTRYPOINT cargo run --release
