FROM rust:1.44.1-stretch

WORKDIR /app
COPY rust_thruster_bench /app
COPY proto /app/proto

ENV RUSTFLAGS "-C target-cpu=native"
RUN cargo build --release

CMD ["./target/release/helloworld-server"]
