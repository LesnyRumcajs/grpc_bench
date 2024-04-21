FROM golang:1.22.2-bookworm

WORKDIR /app
COPY go_connect_bench /app
COPY proto /app/proto

RUN apt update && apt install -y protobuf-compiler
RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@latest && go install connectrpc.com/connect/cmd/protoc-gen-connect-go@latest
RUN protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --go_out=/app/ --connect-go_out=/app/
RUN sed -i 's/proto\/helloworld/local\/proto\/helloworld/g' proto/helloworld/helloworldconnect/helloworld.connect.go
RUN go mod tidy && go build ./... && go install ./...

ENTRYPOINT example
