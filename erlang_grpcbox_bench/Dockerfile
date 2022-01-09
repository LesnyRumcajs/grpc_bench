# syntax=docker.io/docker/dockerfile:1@sha256:42399d4635eddd7a9b8a24be879d2f9a930d0ed040a61324cfdf59ef1357b3b2

FROM --platform=$BUILDPLATFORM docker.io/library/erlang:24.2.0.0@sha256:e4269291a69148b5f9b064c209dfd73ec9f805a09f91fec103282aa4d9899a34 AS erlang
FROM --platform=$BUILDPLATFORM docker.io/library/erlang:24.2.0.0-slim@sha256:35030c81e286c332efe2240de4899a9d25cef629f379aeb2ec83b5b27da8d0f6 AS erlang-slim

FROM erlang as builder
WORKDIR /src/app
COPY erlang_grpcbox_bench/rebar.* .
RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
    set -x \
 && rebar3 --version \
 && rebar3 do get-deps, compile
COPY proto proto
RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
    set -x \
 && rebar3 do grpc gen, compile, xref
COPY erlang_grpcbox_bench/config config
COPY erlang_grpcbox_bench/src/* src/
RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
    set -x \
 && rebar3 do compile, xref \
 && rebar3 as prod tar \
 && mkdir -p /opt/rel \
 && tar -zxf _build/prod/rel/*/*.tar.gz -C /opt/rel


FROM erlang-slim AS release
WORKDIR /app
COPY --from=builder /opt/rel .
EXPOSE 50051
ENTRYPOINT ["/app/bin/erlang_grpcbox_bench"]
CMD ["foreground"]
