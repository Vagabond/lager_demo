A simple little riak-like application for showing how to use lager tracing.

Compile:

    ./rebar get-deps compile

Starting:

    erl -pa ebin deps/*/ebin -s lager_demo

Things to try:

Prettyprint a record:

    lager_demo:print_record().

Start some load:

    lager_demo:loadgen().

Show all the debug messages:

    lager:set_loglevel(lager_console_backend, debug).

Reset the loglevel because your console is flooding:

    lager:set_loglevel(lager_console_backend, debug).

Show all the read-repair operations:

    lager:trace_console([{operation, repair}]).

Reset the traces:

    lager:clear_all_traces().

Show all the PUTs against vnode 13:

    lager:trace_console([{operation, put}, {id, 13}]).

Show all the keys being read between 500 and 600:

    lager:trace_console([{operation, get}, {key '>', 500}, {key, '<', 600}]).

Trace specific GET/PUT operstions:

    lager_demo:traced_get(Key).
    lager_demo:traced_put(Key).

Manually erase a vnode:

    lager_demo_worker:wipe(VnodeID).
