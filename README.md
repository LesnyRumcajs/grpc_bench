![CI](https://github.com/LesnyRumcajs/grpc_bench/actions/workflows/build.yml/badge.svg)
[<img alt="Discord" src="https://img.shields.io/discord/932736311733915668.svg?style=for-the-badge&label=Discord&logo=discord" height="20">](https://discord.gg/NAjC2RRdPg)

One repo to finally have a clear, objective gRPC benchmark with code for everyone to verify and improve.

Contributions are most welcome!

See [Nexthink blog post](https://www.nexthink.com/blog/comparing-grpc-performance/) for a deeper overview of the project and recent results.

# Goal

The goal of this benchmark is to compare the performance and resource usage of various gRPC libraries across different programming languages and technologies. To achieve that, a minimal protobuf contract is used to not pollute the results with other concepts (e.g. performances of hash maps) and to make the implementations simple.

That being said, the service implementations should **NOT** take advantage of that and keep the code generic and maintainable. What does generic mean? One should be able to easily adapt the existing code to some fundamental use cases (e.g. having a thread-safe hash map on server side to provide values to client given some key, performing blocking I/O or retrieving a network resource).\
Keep in mind the following guidelines:
- No inline assembly or other, language specific, tricks / hacks should be used
- The code should be (reasonably) idiomatic, built upon the modern patterns of the language
- Don't make any assumption on the kind of work done inside the server's request handler
- Don't assume all client requests will have the exact same content

# You decide what is better

Although in the end results are sorted according to the number of requests served, one should go beyond and look at the resource usage - perhaps one implementation is slightly better in terms of raw speed but uses three times more CPU to achieve that. Maybe it's better to take the first one if you're running on a Raspberry Pi and want to get the most of it. Maybe it's better to use the latter in a big server with 32 CPUs because it scales. It all depends on your use case. This benchmark is created to help people make an informed decision (and get ecstatic when their favourite technology seems really good, without doubts).

# Metrics

We try to provide some metrics to make this decision easier:

* req/s - the number of requests the service was able to successfully serve
* average latency, and 90/95/99 percentiles - time from sending a request to receiving the response
* average CPU, memory - average resource usage during the benchmark, as reported by `docker stats`

# What this benchmark does NOT take into account

1. Completeness of the gRPC library. We test only basic unary RPC at the moment. This is the most common service method which may be enough for some business use cases, but not for the others. When you're happy about the results of some technology, you should check out it's documentation (if it exists) and decide yourself if is it production-ready.
2. Taste. Some may find beauty in Ruby, some may feel like Java is the only real deal. Others treat languages as tools and don't care at all. We don't judge (officially 😉 ). Unless it's a huge state machine with raw `void` pointers. Ups!


# Prerequisites
Linux or MacOS with Docker. Keep in mind that the results on MacOS may not be that reliable, Docker for Mac runs on a VM.

# Running benchmark
To build the benchmarks images use: `./build.sh [BENCH1] [BENCH2] ...` . You need them to run the benchmarks.

To run the benchmarks use: `./bench.sh [BENCH1] [BENCH2] ...` . They will be run sequentially.

To clean-up the benchmark images use: `./clean.sh [BENCH1] [BENCH2] ...`

## Configuring the benchmark
The benchmark can be configured through the following environment variables:

|**Name**|**Description**|**Default value**|
|--------|---------------|:---------------:|
|GRPC_BENCHMARK_DURATION|Duration of the benchmark.|20s|
|GRPC_BENCHMARK_WARMUP|Duration of the warmup. Stats won't be collected.|5s|
|GRPC_REQUEST_SCENARIO|Scenario (from [scenarios/](scenarios/)) containing the protobuf and the data to be sent in the client request.|complex_proto|
|GRPC_SERVER_CPUS|Maximum number of cpus used by the server.|1|
|GRPC_SERVER_RAM|Maximum memory used by the server.|512m|
|GRPC_CLIENT_CONNECTIONS|Number of connections to use.|50|
|GRPC_CLIENT_CONCURRENCY|Number of requests to run concurrently. It can't be smaller than the number of connections.|1000|
|GRPC_CLIENT_QPS|Rate limit, in queries per second (QPS).|0 (*unlimited*)|
|GRPC_CLIENT_CPUS|Maximum number of cpus used by the client.|1|
|GRPC_IMAGE_NAME|Name of Docker image built by `./build.sh`|`'grpc_bench'`|

### Parameter recommendations
* `GRPC_BENCHMARK_DURATION` should not be too small. Some implementations need a *warm-up* before achieving their optimal performance and most real-life gRPC services are expected to be long running processes. From what we measured, **300s** should be enough.
* `GRPC_SERVER_CPUS` + `GRPC_CLIENT_CPUS` should not exceed total number of cores on the machine. The reason for this is that you don't want the `ghz` client to steal precious CPU cycles from the service under test. Keep in mind that having the `GRPC_CLIENT_CPUS` too low may not saturate the service in some of the more performant implementations. Also keep in mind limiting the number of `GRPC_SERVER_CPUS` to 1 will severely hamper the performance for some technologies - is running a service on 1 CPU your use case? It may be, but keep in mind eventual load balancer also incurs some costs.
* `GRPC_REQUEST_SCENARIO` is a parameter to both `build.sh` and `bench.sh`. The images must be rebuilt each time you intend to use a scenario having a different `helloworld.proto` from the one ran previously.

Other parameters will depend on your use-case. Choose wisely.

# Results
You can find our sample results in the [Wiki](https://github.com/LesnyRumcajs/grpc_bench/wiki). Be sure to run the benchmarks yourself if you have sufficient hardware, especially for multi-core scenarios.


