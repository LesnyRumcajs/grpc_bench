%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service helloworld.Greeter.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2021-05-02T13:23:03+00:00 and should not be modified manually

-module(helloworld_greeter_bhvr).

%% @doc Unary RPC
-callback say_hello(ctx:ctx(), helloworld_pb:hello_request()) ->
    {ok, helloworld_pb:hello_reply(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

