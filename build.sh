#!/bin/sh

docker build --force-rm -f rust_tonic/Dockerfile -t rust_tonic_test .

docker build --force-rm -f rust_thruster/Dockerfile -t rust_thruster_test .

docker build --force-rm -f go_grpc/Dockerfile -t go_grpc_test .

docker build --force-rm -f cpp_grpc/Dockerfile -t cpp_grpc_test .

docker build --force-rm -f cpp_grpc_st/Dockerfile -t cpp_grpc_st_test .

docker build --force-rm -f ruby_grpc/Dockerfile -t ruby_grpc_test .

docker build --force-rm -f python_grpc/Dockerfile -t python_grpc_test .

docker build --force-rm -f scala_akka/Dockerfile -t scala_akka_test .

docker build --force-rm -f java_grpc/Dockerfile -t java_grpc_test .

docker build --force-rm -f kotlin_grpc/Dockerfile -t kotlin_grpc_test .

docker build --force-rm -f crystal_grpc/Dockerfile -t crystal_grpc_test .

docker build --force-rm -f dart_grpc/Dockerfile -t dart_grpc_test .

docker build --force-rm -f java_micronaut/Dockerfile -t java_micronaut_test .

docker build --force-rm -f swift_grpc/Dockerfile -t swift_grpc_test .

docker build --force-rm -f lua_grpc/Dockerfile -t lua_grpc_test .

docker build --force-rm -f node_grpc/Dockerfile -t node_grpc_test .

docker build --force-rm -f php_grpc/Dockerfile -t php_grpc_test .

docker build --force-rm -f csharp_grpc/Dockerfile -t csharp_grpc_test .

docker build --force-rm -f elixir_grpc/Dockerfile -t elixir_grpc_test .

docker build --force-rm -f java_aot/Dockerfile -t java_aot_test .
