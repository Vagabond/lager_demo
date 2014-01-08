-module(lager_demo_worker).
-behaviour(gen_server).

-export([start_link/1, get/3, put/4, repair/4, wipe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          id,
          databass = dict:new()
         }).

get(ID, ReqID, Key) ->
    Name = list_to_atom(lists:flatten(io_lib:format("lager_demo_~b", [ID]))),
    gen_server:call(Name, {get, ReqID, Key}).

put(ID, ReqID, Key, Value) ->
    Name = list_to_atom(lists:flatten(io_lib:format("lager_demo_~b", [ID]))),
    gen_server:call(Name, {put, ReqID, Key, Value}).

repair(ID, ReqID, Key, Value) ->
    Name = list_to_atom(lists:flatten(io_lib:format("lager_demo_~b", [ID]))),
    gen_server:call(Name, {repair, ReqID, Key, Value}).

wipe(ID) ->
    Name = list_to_atom(lists:flatten(io_lib:format("lager_demo_~b", [ID]))),
    gen_server:call(Name, wipe).

start_link(ID) ->
    Name = list_to_atom(lists:flatten(io_lib:format("lager_demo_~b", [ID]))),
    gen_server:start_link({local, Name}, ?MODULE, [ID], []).

init([ID]) ->
    lager:md([{id, ID}]),
    lager:debug("Started worker ID %b", [ID]),
    {ok, #state{id=ID}}.

handle_call({get, ReqID, Key}, _From, State) ->
    Res = case dict:find(Key, State#state.databass) of
              error ->
                  not_found;
              {ok, Value} ->
                  Value
          end,
    lager:debug([{request, ReqID}, {operation, get}, {key, Key}],
                "~b: Got key ~p with value ~p", [State#state.id, Key, Res]),
    {reply, {ok, Res}, State};
handle_call({put, ReqID, Key, Value}, _From, State) ->
    DB = dict:store(Key, Value, State#state.databass),
    lager:debug([{request, ReqID}, {operation, put}, {key, Key}],
                "~b: Put key ~p with value ~p", [State#state.id, Key, Value]),
    {reply, ok, State#state{databass = DB}};
handle_call({repair, ReqID, Key, Value}, _From, State) ->
    DB = dict:store(Key, Value, State#state.databass),
    lager:debug([{request, ReqID}, {operation, repair}, {key, Key}],
                "~b: Repaired key ~p with value ~p", [State#state.id, Key, Value]),
    {reply, ok, State#state{databass = DB}};
handle_call(wipe, _From, State) ->
    {reply, ok, State#state{databass = dict:new()}};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
