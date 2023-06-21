FROM python:3.11-slim

WORKDIR /app
COPY python_async_grpc_bench /app
COPY proto /app/proto

RUN python -m pip install grpcio grpcio-tools asyncio
RUN python -m grpc_tools.protoc -I/app/proto/helloworld --python_out=. --grpc_python_out=. helloworld.proto

ENTRYPOINT [ "python", "/app/server.py" ]
