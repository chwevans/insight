-module(insight_SUITE).

-export([
  init_per_suite/1,
  all/0,
  groups/0
]).

-export([
  timeout_works/1,
  brutal_kill_works/1,
  spawn_completes/1,
  spawns_can_timeout/1,
  short_circuit_modes_work/1
]).

-export([
  noop/3
]).

all() -> [{group, all}].

groups() -> [{all, [parallel, shuffle], [
  timeout_works,
  brutal_kill_works,
  spawn_completes,
  spawns_can_timeout,
  short_circuit_modes_work
]}].

init_per_suite(Config) ->
  application:ensure_all_started(insight),
  Config.

noop(_Type, _Label, _Delta) -> ok.

timeout_works(_Config) ->
  {error, timeout} = insight:measure(fun() ->
    timer:sleep(50)
  end, #{timeout => 1, label => <<"timeout_works">>, metric_function => {?MODULE, noop}}),

  5 = insight:measure(fun() ->
    5
  end, #{timeout => 5, label => <<"timeout_works">>, metric_function => {?MODULE, noop}}).

brutal_kill_works(_Config) ->
  {error, timeout} = insight:measure(fun() ->
    timer:sleep(50)
  end, #{timeout => 1, label => <<"brutal_kill_works">>, brutal_kill => true}),

  [] = insight_metrics:all(reductions, <<"brutal_kill_works">>),

  % We should record metrics here since we didn't kill the spawned process
  {error, timeout} = insight:measure(fun() ->
    timer:sleep(50)
  end, #{timeout => 1, label => <<"brutal_kill_works">>}),
  timer:sleep(50),
  [_] = insight_metrics:all(reductions, <<"brutal_kill_works">>).

spawn_completes(_Config) ->
  ok = insight:measure(fun() ->
    timer:sleep(50),
    5
  end, #{spawn => true, label => <<"spawn_completes">>}),
  [] = insight_metrics:all(reductions, <<"spawn_completes">>),
  timer:sleep(100),
  [_] = insight_metrics:all(reductions, <<"spawn_completes">>).

spawns_can_timeout(_Config) ->
  % Note that we need to brutal_kill, otherwise the timeout will just let a spawned process continue
  ok = insight:measure(fun() ->
    timer:sleep(50),
    5
  end, #{spawn => true, timeout => 1, label => <<"spawns_can_timeout">>, brutal_kill => true}),
  [] = insight_metrics:all(reductions, <<"spawns_can_timeout">>),
  timer:sleep(100),
  [] = insight_metrics:all(reductions, <<"spawns_can_timeout">>),

  % This time it should succeed
  ok = insight:measure(fun() ->
    timer:sleep(50),
    5
  end, #{spawn => true, timeout => 100, label => <<"spawns_can_timeout">>, brutal_kill => true}),
  [] = insight_metrics:all(reductions, <<"spawns_can_timeout">>),
  timer:sleep(100),
  [_] = insight_metrics:all(reductions, <<"spawns_can_timeout">>).


short_circuit_modes_work(_Config) ->
  % Note we don't need any labels here because no metrics are collected.
  {error, short_circuited} = insight:measure(fun() -> 5 end, #{short_circuit => true}),
  5 = insight:measure(fun() -> 5 end, #{short_circuit => work}).
