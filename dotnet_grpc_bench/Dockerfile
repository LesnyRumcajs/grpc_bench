FROM mcr.microsoft.com/dotnet/sdk:11.0 AS build

WORKDIR /app
COPY dotnet_grpc_bench /app
COPY proto /proto

RUN dotnet publish -c Release -o /app/publish GreeterServer/GreeterServer.csproj

FROM mcr.microsoft.com/dotnet/aspnet:11.0

WORKDIR /app
COPY --from=build /app/publish .

ENTRYPOINT [ "dotnet", "GreeterServer.dll" ]
