FROM golang:1.27.4-bookworm

WORKDIR /app
COPY go_kitex_grpc_bench /app
COPY proto /app/proto

RUN apt update && apt install -y protobuf-compiler
RUN go install github.com/cloudwego/kitex/tool/cmd/kitex@v0.16.4

RUN kitex -module example -type protobuf -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto
# Kitex skips the doubly nested enum Hello.Pet.Color; add it by hand
RUN cp /app/kitex_patch/pet_color.go /app/kitex_gen/proto/helloworld/pet_color.go
RUN go mod tidy && go build -o /go/bin/example .

ENTRYPOINT example
