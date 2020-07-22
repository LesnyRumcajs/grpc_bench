#!/bin/sh

NAME=$1
REPORT_DIR=${2:-"results"}

GRPC_BENCHMARK_DURATION=${GRPC_BENCHMARK_DURATION:-"30s"}
GRPC_SERVER_CPUS=${GRPC_SERVER_CPUS:-"1"}

echo "==> Running benchmark for ${NAME}..."
mkdir -p "${REPORT_DIR}"
docker run --name "${NAME}" --rm --cpus "${GRPC_SERVER_CPUS}" --network=host -d -t ${NAME}
sleep 5
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
docker container stop "${NAME}"
