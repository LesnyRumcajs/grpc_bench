FROM dart:stable AS build

WORKDIR /app
COPY dart_grpc_bench/pubspec.yaml /app/pubspec.yaml
RUN dart pub get

COPY proto /app/proto
COPY dart_grpc_bench /app

# Ensure packages are still up-to-date if anything has changed
RUN dart pub get --offline \
    && apt update && apt install -y protobuf-compiler \
    && dart pub global activate protoc_plugin \
    && mkdir -p lib/src/generated

RUN protoc --plugin=protoc-gen-dart=$HOME/.pub-cache/bin/protoc-gen-dart \
    --proto_path=/app/proto/helloworld \
    --dart_out=grpc:lib/src/generated \
    -Iproto /app/proto/helloworld/helloworld.proto

RUN dart compile exe bin/server.dart -o bin/server

# ENTRYPOINT [ "/usr/bin/dart", "/app/bin/server.dart" ]

# Build minimal serving image from AOT-compiled `/server` and required system
# libraries and configuration files stored in `/runtime/` from the build stage.
FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/bin/server /app/bin/

# Start server.
EXPOSE 50051
CMD ["/app/bin/server"]