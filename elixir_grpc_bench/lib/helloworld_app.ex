defmodule HelloworldApp do
  use Application

  def start(_type, _args) do
    children = [
      {Falco.Server.Supervisor,
       {Helloworld.Endpoint, 50051, [num_acceptors: 1000, max_connections: 1_000_000]}}
    ]

    opts = [strategy: :one_for_one, name: HelloworldApp]
    Supervisor.start_link(children, opts)
  end
end
