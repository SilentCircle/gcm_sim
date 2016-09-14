#!/bin/sh
# Start webmachine tracer for debug use
cd `dirname $0`
host=$(hostname -f)
node="wmtracer@${host}"
exec erl -name wmtracer_stopper \
    -noshell -noinput -hidden \
    -eval 'Res=rpc:call('$node',init,stop,[]),
    Status = case Res of
        ok -> "stopped\n";
        _ -> "nodedown\n"
    end,
    io:put_chars(Status),
    init:stop().
    '
