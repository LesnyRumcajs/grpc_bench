%% Copyright © 2021 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% -*- coding: utf-8 -*-
-module(egb_handler).
-behavior(helloworld_greeter_bhvr).

-include_lib("kernel/include/logger.hrl").

-export([say_hello/2]).

-define(NOW(), erlang:monotonic_time()).
-define(SINCE(T), erlang:convert_time_unit(?NOW()-T, native, microsecond)).

%% @doc Unary RPC
-spec say_hello(ctx:ctx(), helloworld_pb:hello_request()) ->
          {ok, helloworld_pb:hello_reply(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

say_hello(Ctx, Req) ->
    StartTime = erlang:monotonic_time(),
    ?LOG_DEBUG(#{status=>handling, req=>Req}),

    #{name := Name
     } = Req,

    Rep = #{message => Name
           },

    ?LOG_INFO(#{status=>handled, rep=>Rep, in_us=>?SINCE(StartTime)}),
    {ok, Rep, Ctx}.
