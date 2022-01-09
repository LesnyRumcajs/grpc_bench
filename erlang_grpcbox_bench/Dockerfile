FROM docker.io/library/erlang:24.2.0.0@sha256:e4269291a69148b5f9b064c209dfd73ec9f805a09f91fec103282aa4d9899a34 AS erlang
FROM docker.io/library/erlang:24.2.0.0-slim@sha256:35030c81e286c332efe2240de4899a9d25cef629f379aeb2ec83b5b27da8d0f6 AS erlang-slim

FROM erlang as builder
WORKDIR /app
COPY erlang_grpcbox_bench /app
COPY proto /app/proto
RUN mkdir -p _build/default/lib/erlang_grpcbox_bench/ebin && rebar3 grpc gen --protos=proto/helloworld --force
RUN rebar3 as prod tar
RUN mkdir -p /opt/rel
RUN tar -zxf _build/prod/rel/*/*.tar.gz -C /opt/rel


FROM erlang-slim AS release
WORKDIR /app
COPY --from=builder /opt/rel .
EXPOSE 50051
ENTRYPOINT ["/app/bin/erlang_grpcbox_bench"]
CMD ["foreground"]
