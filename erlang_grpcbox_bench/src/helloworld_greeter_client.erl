%%%-------------------------------------------------------------------
%% @doc Client module for grpc service helloworld.Greeter.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2021-05-02T13:23:03+00:00 and should not be modified manually

-module(helloworld_greeter_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'helloworld.Greeter').
-define(PROTO_MODULE, 'helloworld_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec say_hello(helloworld_pb:hello_request()) ->
          {ok, helloworld_pb:hello_reply(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
say_hello(Input) ->
    say_hello(ctx:new(), Input, #{}).

-spec say_hello(ctx:t() | helloworld_pb:hello_request(), helloworld_pb:hello_request() | grpcbox_client:options()) ->
          {ok, helloworld_pb:hello_reply(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
say_hello(Ctx, Input) when ?is_ctx(Ctx) ->
    say_hello(Ctx, Input, #{});
say_hello(Input, Options) ->
    say_hello(ctx:new(), Input, Options).

-spec say_hello(ctx:t(), helloworld_pb:hello_request(), grpcbox_client:options()) ->
          {ok, helloworld_pb:hello_reply(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
say_hello(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/helloworld.Greeter/SayHello">>, Input, ?DEF(hello_request, hello_reply, <<"helloworld.HelloRequest">>), Options).

