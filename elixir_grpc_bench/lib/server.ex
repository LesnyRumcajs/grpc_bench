defmodule Helloworld.Greeter.Server do
  use Falco.Server, service: Helloworld.Greeter.Service, compressors: [Falco.Compressor.Gzip]

  @spec say_hello(Helloworld.HelloRequest.t(), Falco.Server.Stream.t()) ::
          Helloworld.HelloReply.t()
  def say_hello(request, _stream) do
    Helloworld.HelloReply.new(response: request.request)
  end
end
