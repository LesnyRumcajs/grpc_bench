#!/bin/sh

docker build --force-rm -f rust_tonic/Dockerfile -t rust_tonic_test .

docker build --force-rm -f go_grpc/Dockerfile -t go_grpc_test .

docker build --force-rm -f cpp_grpc/Dockerfile -t cpp_grpc_test .

docker build --force-rm -f ruby_grpc/Dockerfile -t ruby_grpc_test .
