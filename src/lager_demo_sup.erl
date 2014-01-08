-module(lager_demo_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, start_children/1, start_worker/1]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_children(Num) ->
	[supervisor:start_child(?MODULE, [N]) || N <- lists:seq(0, Num)].

start_worker(N) ->
	lager_demo_worker:start_link(N).

%% supervisor.
init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [{?MODULE, {?MODULE, start_worker, []},
		transient, 5000, worker, [?MODULE]}]}}.
