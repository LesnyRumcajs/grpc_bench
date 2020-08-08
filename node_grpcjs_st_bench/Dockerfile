FROM node:stretch-slim

WORKDIR /app
COPY node_grpcjs_st_bench /app
COPY proto /app/proto

RUN npm install

ENTRYPOINT [ "node", "greeter_server.js" ]
