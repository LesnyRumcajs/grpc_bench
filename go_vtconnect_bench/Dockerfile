FROM golang:1.22.2-bookworm

WORKDIR /app
COPY go_vtconnect_bench /app
COPY proto /app/proto

ENV GOBIN /go/bin

RUN apt update && apt install -y protobuf-compiler
RUN go install google.golang.org/protobuf/cmd/protoc-gen-go@v1.33.0
RUN go install connectrpc.com/connect/cmd/protoc-gen-connect-go@v1.16.1
RUN go install github.com/planetscale/vtprotobuf/cmd/protoc-gen-go-vtproto@v0.6.0
RUN protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --go_out=/app/ --connect-go_out=/app/ --go-vtproto_out=/app/ --plugin protoc-gen-go-vtproto="${GOBIN}/protoc-gen-go-vtproto" --go-vtproto_opt=features=marshal+unmarshal+size 
RUN sed -i 's/proto\/helloworld/local\/proto\/helloworld/g' proto/helloworld/helloworldconnect/helloworld.connect.go
RUN go mod tidy && go build ./... && go install ./...

ENTRYPOINT example
