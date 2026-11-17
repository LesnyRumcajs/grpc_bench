// Package main implements a Kitex (gRPC transport) server for Greeter service.
package main

import (
	"context"
	"log"
	"net"

	_ "go.uber.org/automaxprocs"

	pb "example/kitex_gen/proto/helloworld"
	"example/kitex_gen/proto/helloworld/greeter"

	"github.com/cloudwego/kitex/pkg/klog"
	"github.com/cloudwego/kitex/server"
)

// GreeterImpl implements helloworld.Greeter.
type GreeterImpl struct{}

// SayHello implements helloworld.Greeter.SayHello
func (s *GreeterImpl) SayHello(ctx context.Context, req *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Response: req.GetRequest()}, nil
}

func main() {
	klog.SetLevel(klog.LevelWarn)

	addr, err := net.ResolveTCPAddr("tcp", "0.0.0.0:50051")
	if err != nil {
		log.Fatalf("failed to resolve address: %v", err)
	}
	svr := greeter.NewServer(&GreeterImpl{}, server.WithServiceAddr(addr))
	if err := svr.Run(); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
