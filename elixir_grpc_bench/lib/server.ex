defmodule Helloworld.Greeter.Server do
  use GRPC.Server, service: Helloworld.Greeter.Service, compressors: [GRPC.Compressor.Gzip]

  alias Helloworld.HelloReply

  @spec say_hello(Helloworld.HelloRequest.t(), GRPC.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    HelloReply.new(response: request.request)
  end
end
