FROM golang:1.15

WORKDIR /app
COPY go_grpc_bench /app
COPY proto /app/proto

RUN apt update && apt install -y protobuf-compiler
RUN go get -u github.com/golang/protobuf/protoc-gen-go
RUN protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --go_out=plugins=grpc:/app/proto/helloworld
RUN go build ./... && go install ./...

ENTRYPOINT example
