FROM swift:5.7.0 AS builder

WORKDIR /app

# Getting protoc-gen-swift and protoc-gen-grpc-swift
RUN apt update && apt install -y protobuf-compiler git make
RUN git clone --depth 1 https://github.com/grpc/grpc-swift
WORKDIR /app/grpc-swift
RUN make plugins
RUN cp /app/grpc-swift/protoc-* /usr/local/bin

COPY swift_grpc_bench /app
COPY proto /app/proto

WORKDIR /app
RUN protoc --proto_path=/app/proto/helloworld --plugin=/usr/local/bin/protoc-gen-swift --swift_opt=Visibility=Public --swift_out=/app/Sources/Model helloworld.proto
RUN protoc --proto_path=/app/proto/helloworld --plugin=/usr/local/bin/protoc-gen-grpc-swift --grpc-swift_opt=Visibility=Public --grpc-swift_out=/app/Sources/Model helloworld.proto

# Resolve package dependencies early and store them in the build cache,
# for faster subsequent builds.
RUN swift package resolve
RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked

FROM swift:5.7.0
WORKDIR /app
RUN mkdir /app/.build
COPY --from=builder /app/.build /app/.build
ENTRYPOINT [ "/app/.build/release/Server" ]
