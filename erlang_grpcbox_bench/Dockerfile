FROM erlang:23-alpine as builder

WORKDIR /src/app
COPY erlang_grpcbox_bench/rebar.* .

RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
  --mount=type=cache,target=/var/cache/apk ln -vs /var/cache/apk /etc/apk/cache && \
    set -x \
 && apk update && apk upgrade \
 && apk add git emacs-nox \
 && git version \
 && rebar3 --version \
 && rebar3 do get-deps, compile

COPY proto proto
COPY erlang_grpcbox_bench/config config
COPY erlang_grpcbox_bench/src src

RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
    set -x \
 && git init \
 && git add -A . \
 && rebar3 do grpc gen, fmt \
 && git --no-pager diff && [[ 0 -eq $(git --no-pager diff --name-only | wc -l) ]]

RUN \
  --mount=type=cache,target=/root/.cache/rebar3 \
    set -x \
 && rebar3 as prod tar \
 && mkdir -p /opt/rel \
 && tar -zxf $PWD/_build/prod/rel/*/*.tar.gz -C /opt/rel

# ---

FROM alpine:3.13

WORKDIR /app
COPY --from=builder /opt/rel .

RUN set -x \
 && apk update && apk upgrade \
 && apk add openssl-dev ncurses \
 && rm -r /var/cache/apk/* \
 && ln -s $PWD/bin/erlang_grpcbox_bench $PWD/bin/rel

EXPOSE 50051

ENTRYPOINT ["/app/bin/rel"]
CMD ["foreground"]
