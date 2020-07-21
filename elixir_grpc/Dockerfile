FROM elixir:slim

WORKDIR /app
COPY elixir_grpc /app

RUN apt update && apt install -y git
RUN mix local.hex --force
RUN mix local.rebar --force
ENV MIX_ENV=prod
RUN mix do deps.get, compile


ENTRYPOINT [ "mix", "grpc.server" ]
