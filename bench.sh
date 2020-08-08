#!/bin/sh

## The list of benchmarks to run
BENCHMARKS_TO_RUN="${@}"
##  ...or use all the *_bench dirs by default
BENCHMARKS_TO_RUN="${BENCHMARKS_TO_RUN:-$(find . -name '*_bench' -type d -maxdepth 1)}"

RESULTS_DIR="results/$(date '+%y%d%mT%H%M%S')"
GRPC_BENCHMARK_DURATION=${GRPC_BENCHMARK_DURATION:-"30s"}
GRPC_SERVER_CPUS=${GRPC_SERVER_CPUS:-"1"}

docker pull infoblox/ghz:0.0.1

for benchmark in ${BENCHMARKS_TO_RUN}; do
	NAME="${benchmark##*/}"
	echo "==> Running benchmark for ${NAME}..."

	mkdir -p "${RESULTS_DIR}"
	docker run --name "${NAME}" --rm --cpus "${GRPC_SERVER_CPUS}" \
		--network=host --detach --tty "${NAME}"
	sleep 5
	./collect_stats.sh "${NAME}" "${RESULTS_DIR}" &
	docker run --name ghz --rm --network=host -v "${PWD}/proto:/proto:ro" \
		--entrypoint=ghz infoblox/ghz:0.0.1 \
		--proto=/proto/helloworld/helloworld.proto \
		--call=helloworld.Greeter.SayHello \
		--insecure \
		--connections=5 \
		--duration "${GRPC_BENCHMARK_DURATION}" \
		-d "{\"name\":\"it's not as performant as we expected\"}" \
		127.0.0.1:50051 > "${RESULTS_DIR}/${NAME}".report
	cat "${RESULTS_DIR}/${NAME}".report | grep "Requests/sec"

	kill -INT %1 2>/dev/null
	docker container stop "${NAME}"
done

echo "-----"
echo "Benchmark finished. Detailed results are located in: ${RESULTS_DIR}"
docker run --name analyzer --rm \
	-v "${PWD}/analyze:/analyze:ro" \
	-v "${PWD}/${RESULTS_DIR}:/reports:ro" \
	ruby:2.7-buster ruby /analyze/results_analyze.rb reports

echo "All done."