![CI](https://github.com/LesnyRumcajs/grpc_bench/workflows/CI/badge.svg)

One repo to finally have a clear, objective gRPC benchmark with code for everyone to verify and improve.

Contributions are most welcome!

# Prerequisites
Linux or MacOS with Docker. Keep in mind that the results on MacOS may not be that reliable, Docker for Mac runs on a VM.

# Running benchmark
To build the test images, run: `./build.sh` . You need them to run the benchmarks.

To run all the benchmarks, run: `./bench.sh` . They will be run sequentially.

To run a single benchmark, run: `./run_single_bench.sh <BENCHMARK_NAME>` .

To clean-up the test images, run: `./clean.sh` .

