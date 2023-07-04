FROM docker.io/library/erlang:26.0.1@sha256:c42d00aff6fae5206b47a8868698fe1edfdc8e08d3bfa35c786833e8f72ba638 as builder

WORKDIR /app
COPY erlang_grpcbox_bench /app
COPY proto /app/proto
RUN mkdir -p _build/default/lib/erlang_grpcbox_bench/ebin && rebar3 grpc gen --protos=proto/helloworld --force
RUN rebar3 as prod tar
RUN mkdir -p /opt/rel
RUN tar -zxf _build/prod/rel/*/*.tar.gz -C /opt/rel


FROM debian:bullseye AS release
WORKDIR /app
COPY --from=builder /opt/rel .
EXPOSE 50051
ENTRYPOINT ["/app/bin/erlang_grpcbox_bench"]
CMD ["foreground"]
