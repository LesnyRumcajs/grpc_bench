#!/bin/sh

## The list of benchmarks to run
BENCHMARKS_TO_RUN="${@}"
##  ...or use all the *_bench dirs by default
BENCHMARKS_TO_RUN="${BENCHMARKS_TO_RUN:-$(find . -maxdepth 1 -name '*_bench' -type d | sort)}"

RESULTS_DIR="results/$(date '+%y%d%mT%H%M%S')"
GRPC_BENCHMARK_DURATION=${GRPC_BENCHMARK_DURATION:-"30s"}
GRPC_SERVER_CPUS=${GRPC_SERVER_CPUS:-"1"}
GRPC_SERVER_RAM=${GRPC_SERVER_RAM:-"512m"}
GRPC_CLIENT_CONNECTIONS=${GRPC_CLIENT_CONNECTIONS:-"50"}
GRPC_CLIENT_CONCURRENCY=${GRPC_CLIENT_CONCURRENCY:-"1000"}
GRPC_CLIENT_QPS=${GRPC_CLIENT_QPS:-"0"}
GRPC_CLIENT_QPS=$(( GRPC_CLIENT_QPS / GRPC_CLIENT_CONCURRENCY ))
GRPC_CLIENT_CPUS=${GRPC_CLIENT_CPUS:-"1"}
GRPC_REQUEST_PAYLOAD=${GRPC_REQUEST_PAYLOAD:-"100B"}

# Let containers know how many CPUs they will be running on
export GRPC_SERVER_CPUS
export GRPC_CLIENT_CPUS

docker pull infoblox/ghz:0.0.1

# Loop over benchs
for benchmark in ${BENCHMARKS_TO_RUN}; do
	NAME="${benchmark##*/}"
	echo "==> Running benchmark for ${NAME}..."

	mkdir -p "${RESULTS_DIR}"

	# Start the gRPC Server container
	docker run --name "${NAME}" --rm \
		--cpus "${GRPC_SERVER_CPUS}" \
		--memory "${GRPC_SERVER_RAM}" \
		-e GRPC_SERVER_CPUS \
		-e GRPC_SERVER_RAM \
		--network=host --detach --tty "${NAME}" >/dev/null

	# Wait for server to be ready
	sleep 5

	# Warm up the service
	echo -n "Warming up the service for ${GRPC_BENCHMARK_WARMUP}... "
	docker run --name ghz --rm --network=host -v "${PWD}/proto:/proto:ro" \
	    -v "${PWD}/payload:/payload:ro" \
		--cpus $GRPC_CLIENT_CPUS \
		ghz_bench:latest \
		--proto=/proto/helloworld/helloworld.proto \
		--call=helloworld.Greeter.SayHello \
        --insecure \
        --concurrency="${GRPC_CLIENT_CONCURRENCY}" \
        --connections="${GRPC_CLIENT_CONNECTIONS}" \
        --qps="${GRPC_CLIENT_QPS}" \
        --duration "${GRPC_BENCHMARK_WARMUP}" \
        --data-file /payload/"${GRPC_REQUEST_PAYLOAD}" \
		127.0.0.1:50051 > /dev/null

	echo "done."

	# Actual benchmark
	echo -n "Benchmarking now... "

	# Start collecting stats
	./collect_stats.sh "${NAME}" "${RESULTS_DIR}" &

	# Start the gRPC Client
	docker run --name ghz --rm --network=host -v "${PWD}/proto:/proto:ro" \
	    -v "${PWD}/payload:/payload:ro" \
		--cpus $GRPC_CLIENT_CPUS \
		ghz_bench:latest \
		--proto=/proto/helloworld/helloworld.proto \
		--call=helloworld.Greeter.SayHello \
        --insecure \
        --concurrency="${GRPC_CLIENT_CONCURRENCY}" \
        --connections="${GRPC_CLIENT_CONNECTIONS}" \
        --qps="${GRPC_CLIENT_QPS}" \
        --duration "${GRPC_BENCHMARK_DURATION}" \
        --data-file /payload/"${GRPC_REQUEST_PAYLOAD}" \
		127.0.0.1:50051 >"${RESULTS_DIR}/${NAME}".report

	# Show quick summary (reqs/sec)
	cat << EOF
		done.
		Results:
		$(cat "${RESULTS_DIR}/${NAME}".report | grep "Requests/sec" | sed -E 's/^ +/    /')
EOF

	kill -INT %1 2>/dev/null
	docker container stop "${NAME}" >/dev/null
done

tee $RESULTS_DIR/bench.info <<EOF
Benchmark info:
$(git log -1 --pretty="%h %cD %cn %s")
Benchmarks run: $BENCHMARKS_TO_RUN
GRPC_BENCHMARK_DURATION=$GRPC_BENCHMARK_DURATION
GRPC_SERVER_CPUS=$GRPC_SERVER_CPUS
GRPC_SERVER_RAM=$GRPC_SERVER_RAM
GRPC_CLIENT_CONNECTIONS=$GRPC_CLIENT_CONNECTIONS
GRPC_CLIENT_CONCURRENCY=$GRPC_CLIENT_CONCURRENCY
GRPC_CLIENT_QPS=$GRPC_CLIENT_QPS
GRPC_CLIENT_CPUS=$GRPC_CLIENT_CPUS
GRPC_REQUEST_PAYLOAD=$GRPC_REQUEST_PAYLOAD
EOF

sh analyze.sh $RESULTS_DIR

cat << EOF
$(cat ${RESULTS_DIR}/bench.params)

All done.
EOF
