FROM ruby:3

WORKDIR /app
COPY ruby_grpc_bench /app
COPY proto /app/proto

RUN gem install grpc grpc-tools
RUN mkdir -p /app/lib && grpc_tools_ruby_protoc -I /app/proto/helloworld /app/proto/helloworld/helloworld.proto --ruby_out=/app/lib --grpc_out=/app/lib

ENTRYPOINT ruby server.rb

