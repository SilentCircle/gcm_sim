#!/bin/sh
# Start webmachine tracer for debug use
cd `dirname $0`
exec erl -name wmtracer \
    -noshell -noinput -detached \
    -pa $PWD/apps/gcm_sim/ebin $PWD/deps/*/ebin \
    -boot start_sasl -s reloader -s gcm_sim
