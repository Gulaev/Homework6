%%%-------------------------------------------------------------------
%% @doc homework6 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(homework6_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10},
        [
            {homework6_cache, {homework6_cache, start_link, []},
                permanent, 5000, worker, [homework6_cache]}
        ]}}.
