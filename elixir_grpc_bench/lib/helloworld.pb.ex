defmodule Helloworld.Hello.Pet.Color do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3

  @type t :: integer | :BLACK | :WHITE | :BLUE | :RED | :YELLOW | :GREEN

  field :BLACK, 0
  field :WHITE, 1
  field :BLUE, 2
  field :RED, 3
  field :YELLOW, 4
  field :GREEN, 5
end

defmodule Helloworld.Hello.Pet do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          color: Helloworld.Hello.Pet.Color.t()
        }
  defstruct [:name, :color]

  field :name, 1, type: :string
  field :color, 2, type: Helloworld.Hello.Pet.Color, enum: true
end

defmodule Helloworld.Hello do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          choice: {atom, any},
          name: String.t(),
          d: float | :infinity | :negative_infinity | :nan,
          f: float | :infinity | :negative_infinity | :nan,
          b: boolean,
          n: integer,
          l: integer,
          pets: [Helloworld.Hello.Pet.t()]
        }
  defstruct [:choice, :name, :d, :f, :b, :n, :l, :pets]

  oneof :choice, 0
  field :name, 1, type: :string
  field :d, 2, type: :double
  field :f, 3, type: :float
  field :b, 4, type: :bool
  field :n, 5, type: :int32
  field :l, 6, type: :int64
  field :c1, 7, type: :string, oneof: 0
  field :c2, 8, type: :bool, oneof: 0
  field :pets, 9, repeated: true, type: Helloworld.Hello.Pet
end

defmodule Helloworld.HelloRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          request: Helloworld.Hello.t() | nil
        }
  defstruct [:request]

  field :request, 1, type: Helloworld.Hello
end

defmodule Helloworld.HelloReply do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response: Helloworld.Hello.t() | nil
        }
  defstruct [:response]

  field :response, 1, type: Helloworld.Hello
end

defmodule Helloworld.Greeter.Service do
  @moduledoc false
  use GRPC.Service, name: "helloworld.Greeter"

  rpc :SayHello, Helloworld.HelloRequest, Helloworld.HelloReply
end

defmodule Helloworld.Greeter.Stub do
  @moduledoc false
  use GRPC.Stub, service: Helloworld.Greeter.Service
end
