%%%-------------------------------------------------------------------
%% @doc taskord public API
%% @end
%%%-------------------------------------------------------------------

-module(taskord_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    taskord_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
