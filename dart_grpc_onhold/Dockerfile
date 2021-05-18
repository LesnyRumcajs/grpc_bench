FROM google/dart

WORKDIR /app
COPY dart_grpc_bench/pubspec.yaml /app/pubspec.yaml
COPY proto /app/proto

RUN pub get
COPY dart_grpc_bench /app
RUN pub get --offline
RUN apt update && apt install -y protobuf-compiler
RUN pub global activate protoc_plugin
RUN mkdir -p lib/src/generated
RUN protoc --proto_path=/app/proto/helloworld --dart_out=grpc:lib/src/generated -Iprotos /app/proto/helloworld/helloworld.proto

ENTRYPOINT [ "/usr/bin/dart", "/app/bin/server.dart" ]
