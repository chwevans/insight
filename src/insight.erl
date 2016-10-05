-module(insight).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-export([
  measure/2
]).

-type metric() :: reductions | timing | completed.
-type f() :: {atom(), atom(), list(term())} | fun(() -> term()).

-export_type([
  metric/0
]).

-define(METRICS_TO_COLLECT, [reductions]).
-define(ALL_METRICS, [reductions, timing, completed]).
-define(DEFAULT_OPTIONS, #{timeout => undefined, label => undefined, brutal_kill => false, spawn => false, metric_function => {insight_metrics, record}, metrics => ?ALL_METRICS}).

-spec start() -> {ok, list(atom())}.
start() -> application:ensure_all_started(insight).

-spec start(atom(), list()) -> {ok, pid()} | {error, term()}.
start(_Type, _StartArgs) -> insight_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) -> ok.

-spec measure(f(), map()) -> {error, short_circuited | timeout} | ok | term().
measure(Function, #{short_circuit := work}) ->
  work(Function);
measure(_Fucntion, #{short_circuit := true}) ->
  {error, short_circuited};
measure(Function, BaseOptions = #{label := Label}) when is_function(Function) andalso is_binary(Label) ->
  record(Function, BaseOptions);
measure(Function = {M, F, A}, BaseOptions) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
  record(Function, BaseOptions).

record(Function, BaseOptions) ->
  Options = maps:merge(?DEFAULT_OPTIONS, BaseOptions),
  cast(Function, Options).

cast(Function, Options = #{spawn := false}) ->
  timeout(Function, Options);
cast(Function, Options = #{spawn := true}) ->
  spawn(fun() -> timeout(Function, Options) end),
  ok.

timeout(Function, Options = #{timeout := undefined}) ->
  delta_metrics(Function, Options);
timeout(Function, Options = #{timeout := Timeout, brutal_kill := BrutalKill}) when is_integer(Timeout) andalso is_boolean(BrutalKill) ->
  Self = self(),
  Pid = spawn(fun() ->
    Result = delta_metrics(Function, Options),
    Self ! Result
  end),
  receive
    Result -> Result
  after
    Timeout ->
      BrutalKill andalso exit(Pid, kill),
      {error, timeout}
  end.

delta_metrics(Function, Options) ->
  % TODO: Move metric filtering to before we actually record it
  StartMetrics = erlang:process_info(self(), ?METRICS_TO_COLLECT),
  % TODO: Change this to use os:perf_counter(nano_seconds)
  {TimeElapsed, Result} = timer:tc(fun() -> work(Function) end),
  EndMetrics = erlang:process_info(self(), ?METRICS_TO_COLLECT),

  ListDeltas = lists:zipwith(fun({Metric, ValueStart}, {Metric, ValueEnd}) -> {Metric, ValueEnd - ValueStart} end, StartMetrics, EndMetrics),
  AllMetrics = [{completed, 1}, {timing, TimeElapsed} | ListDeltas],
  record_metrics(Function, AllMetrics, Options),

  Result.

work(_Function = {M, F, A}) -> apply(M, F, A);
work(Function) when is_function(Function) -> Function().

record_metrics(Function, Metrics, Options = #{metric_function := {M, F}, metrics := MetricsToRecord}) ->
  Label = label(Function, Options),
  [apply(M, F, [Key, Label, Value]) || {Key, Value} <- Metrics, lists:member(Key, MetricsToRecord)].

label(_Function, _Options = #{label := Label}) when Label =/= undefined -> Label;
label({M, F, _A}, _Options) -> <<(atom_to_binary(M, utf8))/binary, ":", (atom_to_binary(F, utf8))/binary>>.
