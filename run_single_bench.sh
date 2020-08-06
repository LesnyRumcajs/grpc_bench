#!/bin/sh

NAME=$1
REPORT_DIR=${2:-"results"}

GRPC_BENCHMARK_DURATION=${GRPC_BENCHMARK_DURATION:-"600s"}
GRPC_SERVER_CPUS=${GRPC_SERVER_CPUS:-"2"}
GRPC_SERVER_RAM=${GRPC_SERVER_RAM:-"512m"}

echo "==> Running benchmark for ${NAME}..."

mkdir -p "${REPORT_DIR}"

docker run --name "${NAME}" --rm --memory="${GRPC_SERVER_RAM}" --cpus "${GRPC_SERVER_CPUS}" --network=host -d -t ${NAME}

sleep 5

./collect_stats.sh "${NAME}" "${REPORT_DIR}" &

docker run \
    --name ghz \
    --rm \
    --network=host \
    -v "${PWD}"/proto:/proto:ro \
    --entrypoint=ghz \
    infoblox/ghz:0.0.1 \
        --proto=/proto/helloworld/helloworld.proto \
        --call=helloworld.Greeter.SayHello \
        --insecure \
        --connections=5 \
        --duration "${GRPC_BENCHMARK_DURATION}" \
        -d "{\"name\":\"it's not as performant as we expected\"}" \
        127.0.0.1:50051 > "${REPORT_DIR}"/"${NAME}".report

kill -INT %1 2> /dev/null

docker container stop "${NAME}"
