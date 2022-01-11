require "grpc"
require "grpc/http2"

require "./protobufs/helloworld_services.pb"
require "./protobufs/helloworld.pb"

class HelloWorldHandler < Helloworld::Greeter
  def say_hello(request : Helloworld::HelloRequest) : Helloworld::HelloReply
    Helloworld::HelloReply.new(response: request.request)
  end
end

grpc = GRPC::Server.new
grpc << HelloWorldHandler.new

server = HTTP2::ClearTextServer.new([grpc])
server.listen "0.0.0.0", 50051

