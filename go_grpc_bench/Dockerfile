FROM golang:1.17

WORKDIR /app
COPY go_grpc_bench /app
COPY proto /app/proto

RUN apt update && apt install -y protobuf-compiler
RUN go get google.golang.org/protobuf/cmd/protoc-gen-go
RUN go get google.golang.org/grpc/cmd/protoc-gen-go-grpc


RUN mkdir -p /app/vendor
RUN protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --go-grpc_out=/app/ --go_out=/app/
RUN go mod tidy && go mod vendor && go build ./... && go install ./...

ENTRYPOINT example
