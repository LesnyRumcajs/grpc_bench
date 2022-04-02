FROM rust:1.59

WORKDIR /app
COPY rust_thruster_st_bench /app
COPY proto /app/proto

RUN cargo build --release --locked

ENTRYPOINT ["/app/target/release/helloworld-server"]
