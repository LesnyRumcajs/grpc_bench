#!/bin/sh


# Rust Tonic benchmark
docker run --name rust_tonic_test --rm --cpus 1 -p 50051:50051 -d rust_tonic_test
sleep 5
ghz --proto=proto/helloworld/helloworld.proto --call=helloworld.Greeter.SayHello --insecure -n 200000 -d "{ \"name\":\"it's not as performant as we expected\"}" 127.0.0.1:50051
docker container stop rust_tonic_test
