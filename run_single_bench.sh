#!/bin/sh

NAME=$1
REPORT_DIR=${2:-"results"}

echo "==> Running benchmark for ${NAME}..."
mkdir -p "${REPORT_DIR}"
docker run --name "${NAME}" --rm --cpus 1 --network=host -d ${NAME}
sleep 2
docker run --name ghz --rm --network=host -v "${PWD}"/proto:/proto:ro --entrypoint=ghz infoblox/ghz:0.0.1 --proto=/proto/helloworld/helloworld.proto --call=helloworld.Greeter.SayHello --insecure --duration 30s -d "{\"name\":\"it's not as performant as we expected\"}" 127.0.0.1:50051 > "${REPORT_DIR}"/"${NAME}".report
docker container stop "${NAME}"
