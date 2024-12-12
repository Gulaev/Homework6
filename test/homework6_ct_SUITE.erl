%%%-------------------------------------------------------------------
%%% @author denisgulaev
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Homework6 Common Test Module
%%% @end
%%%-------------------------------------------------------------------
-module(homework6_ct_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, create_table_test/1, insert_test/1, lookup_test/1]).

all() -> [create_table_test, insert_test, lookup_test].

init_per_suite(Config) ->
  ok = application:start(homework6),
  Config.

end_per_suite(_Config) ->
  application:stop(homework6).

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

create_table_test(_Config) ->
  ok = homework6_server:create(test_table),
  Info = ets:info(test_table),
  case Info of
    undefined -> erlang:error({test_failed, "Table test_table was not created"});
    _ -> ok
  end.

insert_test(_Config) ->
  ok = homework6_server:insert(test_table, <<"key1">>, <<"value1">>),
  Result = ets:lookup(test_table, <<"key1">>),
  case Result of
    [{<<"key1">>, <<"value1">>, undefined}] -> ok;
    _ ->
      ErrorMsg = io_lib:format("Key ~p was not inserted correctly", [<<"key1">>]),
      erlang:error({test_failed, ErrorMsg})
  end.

lookup_test(_Config) ->
  ok = homework6_server:insert(test_table, <<"key2">>, <<"value2">>, 5),
  timer:sleep(6000),
  Result = homework6_server:lookup(test_table, <<"key2">>),
  case Result of
    undefined -> ok;
    _ ->
      ErrorMsg = io_lib:format("Key ~p did not expire as expected", [<<"key2">>]),
      erlang:error({test_failed, ErrorMsg})
  end.