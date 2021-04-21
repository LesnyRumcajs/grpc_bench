FROM mcr.microsoft.com/dotnet/core/sdk:5.0

WORKDIR /app
COPY csharp_grpc_bench /app
COPY proto /app/proto

RUN dotnet build -c Release Greeter.sln

ENTRYPOINT [ "dotnet", "run" , "-c", "Release", "-p", "GreeterServer", "--no-build" ]
