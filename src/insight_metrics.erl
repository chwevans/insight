-module(insight_metrics).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  record/3,
  all/2,
  mean/2,
  median/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type state() :: ets:tid().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec record(insight:metric(), binary(), pos_integer()) -> ok.
record(completed, _Label, 1) ->
  ok;
record(Type, Label, Amount) ->
  Timestamp = os:system_time(micro_seconds),
  ets:insert(?MODULE, {{{Type, Label}, Timestamp}, Amount}).

-spec all(insight:metric(), binary()) -> list(integer()).
all(Type, Label) ->
  ets:select(?MODULE, [{
    {{{Type, Label}, '$2'}, '$3'},
    % No filter we want everything
    [],
    ['$3']
  }]).

-spec mean(insight:metric(), binary()) -> float().
mean(Type, Label) ->
  All = all(Type, Label),
  lists:sum(All) / float(length(All)).

-spec median(insight:metric(), binary()) -> integer() | {error, undefined}.
median(Type, Label) ->
  All = lists:sort(all(Type, Label)),
  case All of
    [] -> {error, undefined};
    All -> lists:nth(round(length(All) / 2), All)
  end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(undefined) -> {ok, state()}.
init(undefined) ->
  Tid = ets:new(?MODULE, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, Tid}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

