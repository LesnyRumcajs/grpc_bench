FROM mcr.microsoft.com/dotnet/sdk:11.0 AS builder

RUN apt-get update && apt-get install -y clang zlib1g-dev

WORKDIR /app
COPY dotnet_grpc_aot_bench /app
COPY proto /proto

RUN dotnet publish GreeterServer/GreeterServer.csproj -c Release -o /app/out

FROM mcr.microsoft.com/dotnet/runtime-deps:11.0
WORKDIR /app
COPY --from=builder /app/out/GreeterServer /app/GreeterServer
ENTRYPOINT [ "/app/GreeterServer" ]
