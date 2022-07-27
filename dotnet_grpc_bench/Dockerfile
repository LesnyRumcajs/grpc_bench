FROM mcr.microsoft.com/dotnet/nightly/sdk:7.0

WORKDIR /app
COPY dotnet_grpc_bench /app
COPY proto /proto

RUN dotnet build -c Release Greeter.sln

ENV DOTNET_ReadyToRun=0
ENV DOTNET_TieredPGO=1
ENV DOTNET_TC_QuickJitForLoops=1

ENTRYPOINT [ "dotnet", "run" , "-c", "Release", "-p", "GreeterServer", "--no-build" ]
