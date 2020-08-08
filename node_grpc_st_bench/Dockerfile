FROM node:stretch-slim

WORKDIR /app
COPY node_grpc_st /app
COPY proto /app/proto

RUN npm install

ENTRYPOINT [ "node", "greeter_server.js" ]
