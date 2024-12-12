%%%-------------------------------------------------------------------
%%% @author denisgulaev
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Homework6 Cache Module with Periodic Cleanup
%%% @end
%%%-------------------------------------------------------------------
-module(homework6_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1, insert/3, insert/4, lookup/2]).

%% API
start_link() ->
  gen_server:start_link({local, homework6_server}, ?MODULE, [], []).

create(TableName) ->
  gen_server:call(homework6_server, {create, TableName}).

insert(TableName, Key, Value) ->
  gen_server:call(homework6_server, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, Lifetime) ->
  gen_server:call(homework6_server, {insert_ttl, TableName, Key, Value, Lifetime}).

lookup(TableName, Key) ->
  gen_server:call(homework6_server, {lookup, TableName, Key}).

%% gen_server callbacks
init([]) ->
  {ok, #{table_name => undefined}}.

handle_call({create, TableName}, _From, State) ->
  ets:new(TableName, [named_table, public, set]),
  {reply, ok, State#{table_name => TableName}};

handle_call({insert, TableName, Key, Value}, _From, State) ->
  ets:insert(TableName, {Key, Value, undefined}),
  {reply, ok, State};

handle_call({insert_ttl, TableName, Key, Value, Lifetime}, _From, State) ->
  Timestamp = calendar:universal_time(),
  ExpiryTime = universal_time_to_seconds(Timestamp) + Lifetime,
  ets:insert(TableName, {Key, Value, ExpiryTime}),
  {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
  case ets:lookup(TableName, Key) of
    [] -> {reply, undefined, State};
    [{Key, Value, Timestamp}] ->
      case Timestamp of
        undefined -> {reply, Value, State};
        ExpiryTime ->
          CurrentTime = universal_time_to_seconds(calendar:universal_time()),
          if
            CurrentTime =< ExpiryTime -> {reply, Value, State};
            true -> {reply, undefined, State}
          end
      end
  end.

handle_info(cleanup, State) ->
  TableName = maps:get(table_name, State, undefined),
  case ets:info(TableName) of
    undefined ->
      {noreply, State};
    _ ->
      CurrentTime = universal_time_to_seconds(calendar:universal_time()),
      ets:foldl(fun({Key, _Value, ExpiryTime}, _) ->
        if ExpiryTime =/= undefined, ExpiryTime < CurrentTime ->
          ets:delete(TableName, Key);
          true -> ok
        end
                end, ok, TableName),
      %% Запускаємо наступне очищення через 60 секунд
      erlang:send_after(60000, self(), cleanup),
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Допоміжна функція
universal_time_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}).


handle_cast(_Msg, State) ->
  {noreply, State}.