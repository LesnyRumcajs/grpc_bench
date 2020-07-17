#!/bin/sh

NAME=$1

echo "==> Running benchmark for ${NAME}..."
docker run --name ${NAME} --rm --cpus 1 --network=host -d ${NAME}
sleep 2
docker run --name ghz --rm --network=host -v ${PWD}/proto:/proto:ro --entrypoint=ghz infoblox/ghz:0.0.1 --proto=/proto/helloworld/helloworld.proto --call=helloworld.Greeter.SayHello --insecure --duration 30s -d "{\"name\":\"it's not as performant as we expected\"}" 127.0.0.1:50051
docker container stop ${NAME}
