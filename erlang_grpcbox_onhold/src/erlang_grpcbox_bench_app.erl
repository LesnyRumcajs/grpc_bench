%%%-------------------------------------------------------------------
%% @doc erlang_grpcbox_bench public API
%% @end
%%%-------------------------------------------------------------------
-module(erlang_grpcbox_bench_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_grpcbox_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
