#!/bin/sh
# This is a sample benchmarking script which will test the implementations with increasing number of available CPUs.

bash build.sh || exit 1
 
export GRPC_BENCHMARK_DURATION=120s
export GRPC_BENCHMARK_WARMUP=30s
export GRPC_SERVER_RAM=4096m
export GRPC_CLIENT_CPUS=11

cpus=0
while [ $cpus -ne 4 ]
do
  cpus=$(($cpus+1))
  echo "Benchmarking for $cpus CPU(s)"
  GRPC_SERVER_CPUS=$cpus bash bench.sh || exit 2
  sleep 60
done

echo "Benchmarking finished"
