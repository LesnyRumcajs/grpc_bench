#!/bin/sh
RESULTS_DIR=${RESULTS_DIR:-"${@}"}

echo "-----"
echo "Benchmark finished. Detailed results are located in: ${RESULTS_DIR}"

docker run --name analyzer --rm \
	-v "${PWD}/analyze:/analyze:ro" \
	-v "${PWD}/${RESULTS_DIR}:/reports:ro" \
	ruby:2.7-slim-buster ruby /analyze/results_analyze.rb reports ||
	exit 1

cat > ${RESULTS_DIR}/bench.params << EOF
Benchmark Execution Parameters:
$(git log -1 --pretty="%h %cD %cn %s")
- GRPC_BENCHMARK_DURATION=${GRPC_BENCHMARK_DURATION}
- GRPC_BENCHMARK_WARMUP=${GRPC_BENCHMARK_WARMUP}
- GRPC_SERVER_CPUS=${GRPC_SERVER_CPUS}
- GRPC_SERVER_RAM=${GRPC_SERVER_RAM}
- GRPC_CLIENT_CONNECTIONS=${GRPC_CLIENT_CONNECTIONS}
- GRPC_CLIENT_CONCURRENCY=${GRPC_CLIENT_CONCURRENCY}
- GRPC_CLIENT_QPS=${GRPC_CLIENT_QPS}
- GRPC_CLIENT_CPUS=${GRPC_CLIENT_CPUS}
- GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO}
- GRPC_GHZ_TAG=${GRPC_GHZ_TAG}
EOF
