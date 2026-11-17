FROM swift:6.4.2 AS builder

WORKDIR /app

# Getting protoc-gen-swift and protoc-gen-grpc-swift-2
RUN apt update && apt install -y protobuf-compiler git make
RUN git clone --depth 1 --single-branch --branch 1.41.0 https://github.com/apple/swift-protobuf
WORKDIR /app/swift-protobuf
RUN swift build -c release --product protoc-gen-swift
RUN cp .build/release/protoc-gen-swift /usr/local/bin
WORKDIR /app
RUN git clone --depth 1 --single-branch --branch 2.5.0 https://github.com/grpc/grpc-swift-protobuf
WORKDIR /app/grpc-swift-protobuf
RUN swift build -c release --product protoc-gen-grpc-swift-2
RUN cp .build/release/protoc-gen-grpc-swift-2 /usr/local/bin

COPY swift_grpc2_bench /app
COPY proto /app/proto

WORKDIR /app
RUN protoc --proto_path=/app/proto/helloworld --plugin=/usr/local/bin/protoc-gen-swift --swift_opt=Visibility=Public --swift_out=/app/Sources/Model helloworld.proto
RUN protoc --proto_path=/app/proto/helloworld --plugin=/usr/local/bin/protoc-gen-grpc-swift-2 --grpc-swift-2_opt=Visibility=Public --grpc-swift-2_out=/app/Sources/Model helloworld.proto

# Resolve package dependencies early and store them in the build cache,
# for faster subsequent builds.
RUN swift package resolve
RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked

FROM swift:6.4.2
WORKDIR /app
RUN mkdir /app/.build
COPY --from=builder /app/.build /app/.build
ENTRYPOINT [ "/app/.build/release/Server" ]
