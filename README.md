![CI](https://github.com/LesnyRumcajs/grpc_bench/workflows/CI/badge.svg)

One repo to finally have a clear, objective gRPC benchmark with code for everyone to verify and improve.

Contributions are most welcome!

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
|GRPC_BENCHMARK_DURATION|Duration of the benchmark.|30s|
|GRPC_SERVER_CPUS|Maximum number of cpus used by the server.|1|
|GRPC_SERVER_RAM|Maximum memory used by the server.|512m|
|GRPC_CLIENT_CONNECTIONS|Number of connections to use.|5|
|GRPC_CLIENT_CONCURRENCY|Number of requests to run concurrently. It can't be smaller than the number of connections.|50|
|GRPC_CLIENT_QPS|Rate limit, in queries per second (QPS).|0 (*unlimited*)|
|GRPC_CLIENT_CPUS|Maximum number of cpus used by the client.|1|

# Results
You can find our sample results in the [Wiki](https://github.com/LesnyRumcajs/grpc_bench/wiki). Be sure to run the benchmarks yourself if you have sufficient hardware, especially for multi-core scenarios.
