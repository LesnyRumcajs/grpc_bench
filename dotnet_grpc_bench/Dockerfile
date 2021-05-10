FROM mcr.microsoft.com/dotnet/nightly/sdk:6.0

WORKDIR /app
COPY dotnet_grpc_bench /app
COPY proto /proto

RUN dotnet build -c Release Greeter.sln

ENTRYPOINT [ "dotnet", "run" , "-c", "Release", "-p", "GreeterServer", "--no-build" ]
