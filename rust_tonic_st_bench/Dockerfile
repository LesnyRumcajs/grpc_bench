FROM rust:1.55

WORKDIR /app
COPY rust_tonic_st_bench /app
COPY proto /app/proto

RUN rustup component add rustfmt
RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
