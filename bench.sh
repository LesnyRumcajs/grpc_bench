#!/bin/sh


# Rust Tonic benchmark
docker run --name rust_tonic_test --rm --cpus 1 --network=host -d rust_tonic_test
sleep 5
docker run --name ghz --rm --network=host -v ${PWD}/proto:/proto:ro --entrypoint=ghz infoblox/ghz:0.0.1 --proto=/proto/helloworld/helloworld.proto --call=helloworld.Greeter.SayHello --insecure -n 200000 -d "{\"name\":\"it's not as performant as we expected\"}" 127.0.0.1:50051
docker container stop rust_tonic_test
