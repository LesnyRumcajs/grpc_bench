Since Pajamax uses thread-mode, it creates a thread for each
connection, so make sure that

    GRPC_CLIENT_CONNECTIONS >= GRPC_SERVER_CPUS

At the same time, make sure not to overdo it.

Pajamax is very fast, but it also has limitations. It is
in synchronous mode and only used for internal services.
See its [documentation](https://docs.rs/pajamax) for details.

