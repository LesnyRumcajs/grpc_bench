#!/bin/sh

docker build --force-rm -f rust_tonic/Dockerfile -t rust_tonic_test .

docker build --force-rm -f go_grpc/Dockerfile -t go_grpc_test .

docker build --force-rm -f cpp_grpc/Dockerfile -t cpp_grpc_test .

docker build --force-rm -f ruby_grpc/Dockerfile -t ruby_grpc_test .

docker build --force-rm -f python_grpc/Dockerfile -t python_grpc_test .

docker build --force-rm -f scala_akka/Dockerfile -t scala_akka_test .

docker build --force-rm -f java_grpc/Dockerfile -t java_grpc_test .

docker build --force-rm -f kotlin_grpc/Dockerfile -t kotlin_grpc_test .
