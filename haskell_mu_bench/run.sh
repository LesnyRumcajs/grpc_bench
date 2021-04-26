#!/usr/bin/env sh

/root/.cabal/bin/haskell-mu-bench +RTS "-N${GRPC_SERVER_CPUS:-1}"
