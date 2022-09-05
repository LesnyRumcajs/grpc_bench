FROM golang:1.19.0-buster

WORKDIR /app
COPY go_vtgrpc_bench /app
COPY proto /app/proto

ENV GOBIN /go/bin

RUN apt update && apt install -y protobuf-compiler
RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
RUN go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
RUN go install github.com/planetscale/vtprotobuf/cmd/protoc-gen-go-vtproto@latest

RUN protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --go-grpc_out=/app/ --go_out=/app/ --go-vtproto_out=/app/ --plugin protoc-gen-go-vtproto="${GOBIN}/protoc-gen-go-vtproto" --go-vtproto_opt=features=marshal+unmarshal+size 
RUN go mod tidy && go build ./... && go install ./...

ENTRYPOINT example
