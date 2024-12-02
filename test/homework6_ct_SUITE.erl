%%%-------------------------------------------------------------------
%%% @author denisgulaev
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Homework6 Common Test Module
%%% @end
%%%-------------------------------------------------------------------
-module(homework6_ct_SUITE).
-author("denisgulaev").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  test_insert_lookup/1
]).

all() -> [test_insert_lookup].

init_per_suite(Config) ->
  io:format("Starting suite...~n", []),
  application:start(homework6),
  homework6_cache:start_link(),
  Config.

end_per_suite(Config) ->
  io:format("Ending suite...~n", []),
  application:stop(homework6),
  Config.

init_per_testcase(_TestCase, Config) ->
  io:format("Starting test case...~n", []),
  Config.

end_per_testcase(_TestCase, _Config) ->
  io:format("Ending test case...~n", []),
  ok.

test_insert_lookup(_Config) ->
  io:format("Running test_insert_lookup...~n", []),
  ok = homework6_cache:create(test_cache),
  ok = homework6_cache:insert(test_cache, key1, value1, 60),
  Value = homework6_cache:lookup(test_cache, key1),
  ?assertEqual(value1, Value),
  timer:sleep(61000),
  UndefinedValue = homework6_cache:lookup(test_cache, key1),
  ?assertEqual(undefined, UndefinedValue).