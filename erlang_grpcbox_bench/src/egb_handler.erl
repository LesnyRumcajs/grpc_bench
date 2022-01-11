%% Copyright © 2021 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% -*- coding: utf-8 -*-
-module(egb_handler).
-behavior(helloworld_greeter_bhvr).

-export([say_hello/2]).

%% @doc Unary RPC
-spec say_hello(ctx:ctx(), helloworld_pb:hello_request()) ->
          {ok, helloworld_pb:hello_reply(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

say_hello(Ctx, #{request := Request}) ->
    Rep = #{response => Request},
    {ok, Rep, Ctx}.
