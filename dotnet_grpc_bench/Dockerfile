FROM mcr.microsoft.com/dotnet/sdk:9.0

WORKDIR /app
COPY dotnet_grpc_bench /app
COPY proto /proto

RUN dotnet build -c Release Greeter.sln

ENV DOTNET_GCDynamicAdaptationMode=0

ENTRYPOINT [ "dotnet", "run" , "-c", "Release", "-p", "GreeterServer", "--no-build" ]
