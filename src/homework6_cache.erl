%%%-------------------------------------------------------------------
%%% @author denisgulaev
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Homework6 Cache Module with Periodic Cleanup
%%% @end
%%%-------------------------------------------------------------------
-module(homework6_cache).
-author("denisgulaev").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  create/1,
  insert/3,
  insert/4,
  lookup/2,
  delete/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {table_name}).

%%% API Functions
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(TableName) ->
  gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
  gen_server:call(?MODULE, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, TTL) ->
  gen_server:call(?MODULE, {insert, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
  gen_server:call(?MODULE, {lookup, TableName, Key}).

delete(TableName, Key) ->
  gen_server:call(?MODULE, {delete, TableName, Key}).

%%% gen_server Callbacks
init([]) ->
  self() ! delete_obsolete,
  {ok, #state{}}.

handle_call({create, TableName}, _From, State) ->
  try
    ets:new(TableName, [named_table, public, set]),
    {reply, ok, State#state{table_name = TableName}}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

handle_call({insert, TableName, Key, Value}, _From, State) ->
  try
    ets:insert(TableName, {Key, Value}),
    {reply, ok, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

handle_call({insert, TableName, Key, Value, TTL}, _From, State) ->
  try
    CurrentTime = calendar:universal_time_to_seconds(calendar:universal_time()),
    ExpireAt = CurrentTime + TTL,
    ets:insert(TableName, {Key, Value, ExpireAt}),
    {reply, ok, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

handle_call({lookup, TableName, Key}, _From, State) ->
  try
    CurrentTime = calendar:universal_time_to_seconds(calendar:universal_time()),
    Result = case ets:lookup(TableName, Key) of
               [{Key, Value, ExpireAt}] when ExpireAt > CurrentTime ->
                 Value;
               [{Key, Value}] -> % для записів без обмеження часу
                 Value;
               _ ->
                 undefined
             end,
    {reply, Result, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

handle_call({delete, TableName, Key}, _From, State) ->
  try
    ets:delete(TableName, Key),
    {reply, ok, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(delete_obsolete, State) ->
  try
    case State#state.table_name of
      undefined -> ok;
      TableName -> delete_obsolete_table(TableName)
    end,
    erlang:send_after(60000, self(), delete_obsolete),
    {noreply, State}
  catch
    error:Reason ->
      error_logger:error_msg("Error in delete_obsolete: ~p", [Reason]),
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal Functions
delete_obsolete_table(TableName) ->
  CurrentTime = calendar:universal_time_to_seconds(calendar:universal_time()),
  MatchSpec = [{{'$1', '$2', '$3'}, [{'<', '$3', CurrentTime}], ['true']}],
  DeletedCount = ets:select_delete(TableName, MatchSpec),
  error_logger:info_msg("Deleted ~p obsolete entries from table ~p", [DeletedCount, TableName]),
  ok.