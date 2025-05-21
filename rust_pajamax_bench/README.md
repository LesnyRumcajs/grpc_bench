Pajamax is different from most other frameworks which are asynchronous.
Pajamax uses synchronous thread model (which is one of the reasons for
its high performance), and it creates a thread for each incoming connection.
That is to say, at the benchmark, the number of CPUs that pajamax
can utilize depends on the number of concurrent connections,
the `GRPC_CLIENT_CONNECTIONS`.

Therefore, when run the benchmark, notice `GRPC_CLIENT_CONNECTIONS` and
`GRPC_SERVER_CPUS` configrations:

1. If `GRPC_CLIENT_CONNECTIONS < GRPC_SERVER_CPUS`, Pajamax can
   not fully utilize CPUs;

2. If `GRPC_CLIENT_CONNECTIONS >> GRPC_SERVER_CPUS`, each CPU will
   have multiple threads, and switching between threads may affect
   performance;

3. However if `GRPC_CLIENT_CONNECTIONS = GRPC_SERVER_CPUS`, since
   Pajamax is much faster than the benchmark client `ghz`, the Pajamax
   still can not fully utilize CPUs.

4. So setting `GRPC_CLIENT_CONNECTIONS` slightly several times larger
   than `GRPC_SERVER_CPUS` may be the best.

See Pajamax's [documentation](https://docs.rs/pajamax) for details.
