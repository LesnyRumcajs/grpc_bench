FROM swift:5.2

WORKDIR /app

# Resolve package dependencies early and store them in the build cache,
# for faster subsequent builds.
COPY swift_grpc_bench/Package.swift /app
RUN swift package resolve

COPY swift_grpc_bench /app
RUN swift build -c release

ENTRYPOINT [ "/app/.build/release/Server" ]
