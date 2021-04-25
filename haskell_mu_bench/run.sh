#!/usr/bin/env bash

set -euo pipefail

/root/.cabal/bin/haskell-mu-bench +RTS "-N${GRPC_SERVER_CPUS:-1}"
