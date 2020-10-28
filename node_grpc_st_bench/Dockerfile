FROM node:stretch-slim

WORKDIR /app
COPY node_grpc_st_bench /app
COPY proto /app/proto

RUN apt update && apt install -y python3 make g++
RUN npm install

ENTRYPOINT [ "node", "greeter_server.js" ]
