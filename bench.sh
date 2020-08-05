#!/bin/sh

RESULT_DIR=results/$(date "+%y%d%mT%H%M%S")

docker pull infoblox/ghz:0.0.1

GRPC_BENCHMARK_SELECT=${GRPC_BENCHMARK_SELECT:-"."}

for NAME in rust_tonic_mt rust_tonic_st rust_thruster \
            go_grpc \
            cpp_grpc_mt cpp_grpc_st \
            ruby_grpc \
            python_grpc \
            java_grpc java_micronaut java_aot \
            kotlin_grpc \
            scala_akka \
            node_grpc_st node_grpcjs_st \
            dart_grpc \
            crystal_grpc \
            swift_grpc_st \
            csharp_grpc \
            dotnet_grpc \
            lua_grpc_st \
            php_grpc \
            elixir_grpc \
            r_grpc; do
    if echo $NAME | grep -qE "$GRPC_BENCHMARK_SELECT"; then
        ./run_single_bench.sh ${NAME}_test "${RESULT_DIR}"
    fi
done

echo "-----"
echo "Benchmark finished. Detailed results are located in: ${RESULT_DIR}"

docker run --name analyzer --rm \
  -v "${PWD}"/analyze:/analyze:ro \
  -v "${PWD}"/"${RESULT_DIR}":/reports:ro \
  ruby:2.7-buster ruby /analyze/results_analyze.rb reports
