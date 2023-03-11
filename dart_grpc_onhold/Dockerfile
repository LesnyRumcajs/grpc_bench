FROM dart:2.19.4 AS build

WORKDIR /app
COPY dart_grpc_bench/pubspec.yaml /app/pubspec.yaml
COPY proto /app/proto

RUN dart pub get
COPY dart_grpc_bench /app
# Ensure packages are still up-to-date if anything has changed
RUN dart pub get --offline
RUN apt update && apt install -y protobuf-compiler
RUN dart pub global activate protoc_plugin
RUN mkdir -p lib/src/generated
RUN protoc --plugin=protoc-gen-dart=$HOME/.pub-cache/bin/protoc-gen-dart --proto_path=/app/proto/helloworld --dart_out=grpc:lib/src/generated -Iproto /app/proto/helloworld/helloworld.proto

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