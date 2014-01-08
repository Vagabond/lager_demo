-module(lager_demo).

-export([start/0, print_record/0, get/1, traced_get/1, put/2, traced_put/2, loadgen/0, loadgen_stop/0]).

-record(company, {
          name,
          location,
          niche
         }).

start() ->
    application:load(lager),
    application:set_env(lager, color, true),
    lager:start(),
    application:start(lager_demo).

print_record() ->
    Company = #company{name="Basho",
                       location="wherever there is injustice",
                       niche="stripped databass wrangling"},
    lager:info("Lager is brought to you by ~p", [lager:pr(Company, ?MODULE)]).

get(Key) when is_integer(Key) ->
    get(reqid(), Key).

traced_get(Key) when is_integer(Key) ->
    ReqID = reqid(),
    {ok, Trace} = lager:trace_console([{request, ReqID}]),
    Res = get(ReqID, Key),
    lager:stop_trace(Trace),
    Res.

get(ReqID, Key) ->
    Res = [{P, element(2, lager_demo_worker:get(P, ReqID, Key))} || P <- preflist(Key)],
    %% worst sibling resolution ever
    case lists:usort([V || {_, V} <- Res, V /= not_found]) of
        [GoodValue|_] ->
            [lager_demo_worker:repair(P, ReqID, Key, GoodValue) || {P, V} <- Res, V /= GoodValue];
        _ ->
            %% notfound
            ok
    end,
    Res.

put(Key, Value) when is_integer(Key) ->
    put(reqid(), Key, Value).

traced_put(Key, Value) when is_integer(Key) ->
    ReqID = reqid(),
    {ok, Trace} = lager:trace_console([{request, ReqID}]),
    Res = put(ReqID, Key, Value),
    lager:stop_trace(Trace),
    Res.

put(ReqID, Key, Value) when is_integer(Key) ->
    _ = [lager_demo_worker:put(P, ReqID, Key, Value) || P <- preflist(Key)],
    ok.

loadgen() ->
    spawn(fun loadgen_int/0).

loadgen_stop() ->
    case whereis(loadgen) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, kill)
    end.

loadgen_int() ->
    register(loadgen, self()),
    random:seed(now()),
    loadgen_loop().

loadgen_loop() ->
    case random:uniform(10) of
        X when X < 6 ->
            ?MODULE:get(random:uniform(1000));
        X when X < 10 ->
            ?MODULE:put(random:uniform(1000), os:timestamp());
        X when X == 10 ->
            {ok, Size} = application:get_env(lager_demo, ring_size),
            lager_demo_worker:wipe(random:uniform(Size))
    end,
    timer:sleep(100),
    loadgen_loop().

preflist(Key) ->
    {ok, Size} = application:get_env(lager_demo, ring_size),
    P1 = Key rem Size,
    P2 = (P1+1) rem Size,
    P3 = (P2+1) rem Size,
    [P1, P2, P3].

reqid() ->
    erlang:phash2({self(), erlang:now()}).
