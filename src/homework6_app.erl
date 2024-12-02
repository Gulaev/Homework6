%%%-------------------------------------------------------------------
%% @doc homework6 public API
%% @end
%%%-------------------------------------------------------------------

-module(homework6_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    homework6_sup:start_link().

stop(_State) ->
    ok.
