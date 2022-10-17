// Package main implements a server for Greeter service.
package main

import (
	"context"
	helloworld "local/proto/helloworld"
	helloworldconnect "local/proto/helloworld/helloworldconnect"
	"log"
	"net/http"

	"github.com/bufbuild/connect-go"
	_ "go.uber.org/automaxprocs"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

const (
	port = ":50051"
)

// server is used to implement helloworld.GreeterServer.
type server struct {
	helloworldconnect.UnimplementedGreeterHandler
}

// SayHello implements helloworld.GreeterServer
func (s *server) SayHello(ctx context.Context, in *connect.Request[helloworld.HelloRequest]) (*connect.Response[helloworld.HelloReply], error) {
	return connect.NewResponse(&helloworld.HelloReply{Response: in.Msg.GetRequest()}), nil
}

func main() {
	mux := http.NewServeMux()
	mux.Handle(helloworldconnect.NewGreeterHandler(&server{}))
	err := http.ListenAndServe(
		port,
		h2c.NewHandler(mux, &http2.Server{}),
	)
	log.Fatalf("listen failed: %v", err)
}
