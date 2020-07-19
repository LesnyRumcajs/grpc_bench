FROM swift:5.2

WORKDIR /app
COPY swift_grpc /app

RUN swift build -c release

ENTRYPOINT [ "/app/.build/release/Server" ]
