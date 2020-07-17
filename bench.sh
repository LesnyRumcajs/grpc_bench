#!/bin/sh


# Rust Tonic benchmark
./run_single_bench.sh rust_tonic_test

# Go grpc benchmark
./run_single_bench.sh go_grpc_test

# Cpp grpc benchmark
./run_single_bench.sh cpp_grpc_test

# Ruby benchmark
./run_single_bench.sh ruby_grpc_test
