FROM elixir:slim

WORKDIR /app
COPY elixir_grpc_bench /app
COPY proto /app/proto

RUN apt update && apt install -y git protobuf-compiler
RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix escript.install --force hex protobuf
RUN cp /root/.mix/escripts/protoc-gen-elixir /usr/bin/

RUN protoc --proto_path=/app/proto/helloworld --elixir_out=plugins=grpc:./lib/ helloworld.proto

ENV MIX_ENV=prod
RUN mix do clean, deps.clean --all, deps.get, compile

RUN echo "+sbwt none" >> /app/rel/vm.args.eex \
    && echo "+sbwtdcpu none" >> /app/rel/vm.args.eex \
    && echo "+sbwtdio none" >> /app/rel/vm.args.eex \
    && echo "+K true" >> /app/rel/vm.args.eex 

RUN mix release.init \
    && mix release

CMD ["/app/_build/prod/rel/helloworld/bin/helloworld", "start"]