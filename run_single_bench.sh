#!/bin/sh

NAME="${1}"

echo "==> Running benchmark for ${NAME}..."
docker run --name "${NAME}" --network=host --rm --cpus 1 --detach "${NAME}" \
	> /dev/null
sleep 2
docker run --name "ghz" --network=host --rm \
	--volume ${PWD}/proto:/proto:ro \
	infoblox/ghz:0.0.1 ghz \
	--proto=/proto/helloworld/helloworld.proto \
	--call=helloworld.Greeter.SayHello --insecure --duration 30s \
	--data '{"name":"it is not as performant as we expected"}' \
	127.0.0.1:50051
docker container stop ${NAME} > /dev/null
