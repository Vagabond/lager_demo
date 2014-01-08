-module(lager_demo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_, _) ->
	Res = lager_demo_sup:start_link(),
	{ok, Size} = application:get_env(lager_demo, ring_size),
	lager_demo_sup:start_children(Size),
	Res.

stop(_) ->
	ok.
