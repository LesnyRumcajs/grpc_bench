FROM haskell:8.10.4

COPY haskell_mu_bench /app/haskell_mu_bench
COPY proto /app/proto

RUN cd /app/haskell_mu_bench && cabal update && cabal install

ENTRYPOINT /root/.cabal/bin/haskell-mu-bench
