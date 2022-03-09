defmodule Helloworld.Endpoint do
  use Falco.Endpoint

  intercept(Falco.Logger.Server)
  run(Helloworld.Greeter.Server)
end
