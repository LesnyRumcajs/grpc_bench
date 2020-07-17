Benchmarking gRPC in Different Programming Languages
====================================================

One repo to finally have a clear, objective gRPC benchmark with code for everyone to verify and improve.

Contributions are most welcome!

Prerequisites
-------------

Linux or MacOS with Docker. Keep in mind that the results on MacOS may not be that reliable, Docker for Mac runs on a VM.

Running benchmark
-----------------

To build the test images, run: `./build.sh` . You need them to run the benchmarks.

To run all the benchmarks, run: `./bench.sh` . They will be run sequentially.

To run a single benchmark, run: `./run_single_bench.sh <BENCHMARK_NAME>` .

To clean-up the test images, run: `./clean.sh` .

Results
-------

```bash
==> Running benchmark for rust_tonic...

Summary:
  Count:	475210
  Total:	30.01 s
  Slowest:	46.76 ms
  Fastest:	0.16 ms
  Average:	3.11 ms
  Requests/sec:	15834.40

Response time histogram:
  0.155 [1]	|
  4.815 [432823]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  9.475 [39974]	|∎∎∎∎
  14.135 [2122]	|
  18.795 [187]	|
  23.455 [49]	|
  28.115 [0]	|
  32.775 [0]	|
  37.435 [9]	|
  42.095 [21]	|
  46.755 [20]	|

Latency distribution:
  10 % in 1.73 ms
  25 % in 2.22 ms
  50 % in 2.85 ms
  75 % in 3.72 ms
  90 % in 4.68 ms
  95 % in 5.56 ms
  99 % in 8.15 ms

Status code distribution:
  [OK]            475206 responses
  [Canceled]      1 responses
  [Unavailable]   3 responses

Error distribution:
  [3]   rpc error: code = Unavailable desc = transport is closing
  [1]   rpc error: code = Canceled desc = grpc: the client connection is closing

==> Running benchmark for go_grpc...

Summary:
  Count:	435671
  Total:	30.00 s
  Slowest:	29.20 ms
  Fastest:	0.18 ms
  Average:	3.38 ms
  Requests/sec:	14521.80

Response time histogram:
  0.183 [1]	|
  3.084 [226648]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  5.985 [176308]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  8.887 [28061]	|∎∎∎∎∎
  11.788 [3949]	|∎
  14.689 [590]	|
  17.591 [51]	|
  20.492 [2]	|
  23.393 [0]	|
  26.295 [0]	|
  29.196 [50]	|

Latency distribution:
  10 % in 1.80 ms
  25 % in 2.35 ms
  50 % in 3.02 ms
  75 % in 3.99 ms
  90 % in 5.47 ms
  95 % in 6.64 ms
  99 % in 8.97 ms

Status code distribution:
  [OK]            435660 responses
  [Unavailable]   10 responses
  [Canceled]      1 responses

Error distribution:
  [10]   rpc error: code = Unavailable desc = transport is closing
  [1]    rpc error: code = Canceled desc = grpc: the client connection is closing

==> Running benchmark for cpp_grpc...

Summary:
  Count:	465449
  Total:	30.00 s
  Slowest:	256.05 ms
  Fastest:	0.13 ms
  Average:	3.17 ms
  Requests/sec:	15512.52

Response time histogram:
  0.133 [1]	|
  25.724 [465043]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  51.315 [129]	|
  76.907 [120]	|
  102.498 [16]	|
  128.090 [74]	|
  153.681 [0]	|
  179.273 [0]	|
  204.864 [0]	|
  230.456 [11]	|
  256.047 [50]	|

Latency distribution:
  10 % in 1.67 ms
  25 % in 2.19 ms
  50 % in 2.72 ms
  75 % in 3.67 ms
  90 % in 4.89 ms
  95 % in 5.95 ms
  99 % in 8.86 ms

Status code distribution:
  [OK]            465444 responses
  [Canceled]      2 responses
  [Unavailable]   3 responses

Error distribution:
  [3]   rpc error: code = Unavailable desc = transport is closing
  [2]   rpc error: code = Canceled desc = grpc: the client connection is closing

==> Running benchmark for ruby_grpc...

Summary:
  Count:	82527
  Total:	30.01 s
  Slowest:	78.78 ms
  Fastest:	1.60 ms
  Average:	18.09 ms
  Requests/sec:	2750.14

Response time histogram:
  1.601 [1]	|
  9.319 [710]	|∎
  17.036 [24990]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  24.754 [43511]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  32.471 [4595]	|∎∎∎∎
  40.189 [690]	|∎
  47.906 [167]	|
  55.624 [322]	|
  63.341 [301]	|
  71.059 [173]	|
  78.776 [20]	|

Latency distribution:
  10 % in 13.63 ms
  25 % in 16.02 ms
  50 % in 18.52 ms
  75 % in 20.92 ms
  90 % in 24.07 ms
  95 % in 26.86 ms
  99 % in 50.26 ms

Status code distribution:
  [OK]                  75480 responses
  [ResourceExhausted]   6997 responses
  [Unavailable]         49 responses
  [Canceled]            1 responses

Error distribution:
  [49]     rpc error: code = Unavailable desc = transport is closing
  [1]      rpc error: code = Canceled desc = grpc: the client connection is closing
  [6997]   rpc error: code = ResourceExhausted desc = No free threads in thread pool

==> Running benchmark for python_grpc...

Summary:
  Count:	46339
  Total:	30.01 s
  Slowest:	77.02 ms
  Fastest:	2.84 ms
  Average:	32.27 ms
  Requests/sec:	1544.31

Response time histogram:
  2.842 [1]	|
  10.260 [10]	|
  17.677 [232]	|
  25.095 [2411]	|∎∎∎∎
  32.512 [23990]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  39.930 [16052]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  47.347 [2950]	|∎∎∎∎∎
  54.765 [518]	|∎
  62.182 [66]	|
  69.600 [37]	|
  77.017 [22]	|

Latency distribution:
  10 % in 26.72 ms
  25 % in 29.37 ms
  50 % in 31.82 ms
  75 % in 34.58 ms
  90 % in 38.71 ms
  95 % in 41.71 ms
  99 % in 48.70 ms

Status code distribution:
  [OK]            46289 responses
  [Unavailable]   50 responses

Error distribution:
  [50]   rpc error: code = Unavailable desc = transport is closing
```
