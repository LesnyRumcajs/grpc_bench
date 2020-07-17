#!/bin/sh

for benchmark in rust_tonic go_grpc cpp_grpc ruby_grpc python_grpc; do
	./run_single_bench.sh ${benchmark}
done
