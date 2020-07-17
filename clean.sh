#!/bin/sh

docker rmi \
    infoblox/ghz:0.0.1 \
    rust:1.44.1-stretch \
    rust_tonic_test \
    golang:1.14 \
    go_grpc_test \
    gcc:10 \
    cpp_grpc_test
