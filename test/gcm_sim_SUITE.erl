-module(gcm_sim_SUITE).
-author('Edwin Fine <efine@silentcircle.com>').
-compile(export_all).

-record(sim_result, {
        message_id = [],
        registration_id = [],
        error = []
        }).

-record(sim_req, {
        status_code = 200,
        retry_after = 0,
        results = [] :: list(#sim_result{})
        }).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [my_test_case].

suite() ->
    [{timetrap, {seconds, 30}},
     {require, wm_config},
     {require, wm_tracer_node}].

groups() ->
    [].

init_per_suite(Config) ->
    WmConfig = get_wm_config(Config),
    TracerNode = locate_node(ct:get_config(wm_tracer_node)),
    case get_trace_dirs(WmConfig) of
        [_|_] = TraceDirs ->
            wmtracer_setup(TracerNode, TraceDirs);
        [] ->
            ok
    end,
    Env = [{wm_config, WmConfig}],
    ct:log("Starting sim with config: ~p", [Env]),
    ok = gcm_sim:start(Env),
    {ok, _Pid} = inets:start(httpc, [{profile, gcm_sim_ct}]),
    [{wm_config, WmConfig} | Config].

end_per_suite(_Config) ->
    ok = gcm_sim:stop(),
    inets:stop(httpc, gcm_sim_ct),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

my_test_case() ->
    [].

my_test_case(Config) ->
    WmConfig = ?config(wm_config, Config),
    Notification = create_notification(2),
    Req = make_request(WmConfig, Notification),
    ct:log("Req: ~p", [Req]),
    {ok, Res} = httpc:request(post, Req, [], [{body_format, binary}]),
    ct:log("Res: ~p", [Res]),
    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================
pv(K, PL) ->
    proplists:get_value(K, PL).

pv(K, PL, Def) ->
    proplists:get_value(K, PL, Def).

pv_req(K, PL) ->
    case pv(K, PL) of
        undefined ->
            throw({missing_required_property, K});
        Val ->
            Val
    end.

make_url(Host, Port) when is_list(Host) ->
    lists:flatten(io_lib:format("http://~s:~B/gcm/send", [Host, Port])).

make_request(WmConfig, Notification) ->
    IP = pv_req(ip, WmConfig),
    Port = pv_req(port, WmConfig),
    URL = make_url(IP, Port),

    Headers = maybe_debug_hdr() ++
              [{"X-GCMSimulator-StatusCode", "200"},
               {"X-GCMSimulator-Retry-After", "60"},
               {"X-GCMSimulator-Results",
                "message_id:1-12345,registration_id:Kkj665;error:Unavailable"}],

    JSON = jsx:encode(Notification),
    {URL, Headers, "application/json", JSON}.

make_sim_req(PL) ->
    #sim_req{status_code = pv(status_code, PL, 200),
             retry_after = pv(retry_after, PL, 0),
             results     = pv(results, PL, [])}.

get_wm_config(Config) ->
    PrivDir = pv_req(priv_dir, Config),
    WmConfig = set_prop(log_dir, ct:get_config(wm_config), PrivDir),
    case is_debug() of
        true -> % Update webmachine dispatch list to turn tracing on
            TraceDir = filename:join([PrivDir, "wmtrace", "."]),
            ok = filelib:ensure_dir(TraceDir),
            DispatchList0 = pv_req(dispatch, WmConfig),
            DispatchList = update_dispatch_list(DispatchList0, TraceDir),
            set_prop(dispatch, WmConfig, DispatchList);
        false ->
            WmConfig
    end.

get_trace_dirs(WmConfig) ->
    DispatchList = pv(dispatch, WmConfig, []),
    L = [TraceDir || {_, _, Opts} <- DispatchList,
                     is_list(TraceDir = pv(debug, Opts))],
    lists:usort(L). % Eliminate dups

%% @doc Add trace path to all paths in webmachine dispatch list.
update_dispatch_list(DispatchList, TraceDir) ->
    [{PathList, Resource, set_prop(debug, Attrs, TraceDir)} ||
        {PathList, Resource, Attrs} <- DispatchList].

set_prop(K, PL, NewVal) ->
    lists:keystore(K, 1, PL, {K, NewVal}).

create_notification(NumRegIds) when is_integer(NumRegIds) ->
    RegIds = generate_reg_ids(NumRegIds),
    [
        {registration_ids, RegIds},
        {collapse_key, <<"Something">>},
        {data, [{msg, module_name()}]},
        {delay_while_idle, false},
        {time_to_live, 3600},
        {restricted_package_name, <<"foo_pkg">>},
        {dry_run, false}
    ].


generate_req_body(ReqParams) ->
    pv(num_reg_ids, ReqParams, 1).

%% @doc Convert base64 special chars to URL-friendly ones.
-compile({inline, [b64_tr/1]}).
b64_tr($+) -> $-;
b64_tr($/) -> $_;
b64_tr(Ch) -> Ch.

%% @doc Convert a base64 binary string to a URL-friendly one.
%% Strip off `=' chars.
-spec make_url_friendly(binary()) -> binary().
make_url_friendly(<<BStr/binary>>) ->
    << <<(b64_tr(Byte))>> || <<Byte>> <= BStr, Byte =/= $= >>.

%% @doc This creates a random binary string that kind of looks like a Google
%% registration ID.
-spec generate_reg_id() -> binary().
generate_reg_id() ->
    make_url_friendly(base64:encode(crypto:rand_bytes(108))).

%% @doc Generate N random GCM-like registration IDs.
-spec generate_reg_ids(pos_integer()) -> [binary()].
generate_reg_ids(N) when N > 0 ->
    generate_reg_ids(N, []).

-spec generate_reg_ids(non_neg_integer(), [binary()]) -> [binary()].
generate_reg_ids(0, Acc) ->
    Acc;
generate_reg_ids(N, Acc) when N > 0 ->
    generate_reg_ids(N - 1, [generate_reg_id() | Acc]).

module_name() ->
    list_to_binary(atom_to_list(?MODULE)).

maybe_debug_hdr() ->
    [{"X-GCMSimulator-Debug", atom_to_list(is_debug())}].

is_debug() ->
    ct:get_config(debug, false) =/= false. % Anything but false is true here.

%%%--------------------------------------------------------------------
%%% @doc Webmachine tracer setup - tracer node must be running for this to
%%% have any effect. To see trace (if one has been generated), point
%%% a browser to http://localhost:8888/dev/wmtrace.
%%% @end
%%%--------------------------------------------------------------------
wmtracer_setup(TracerNode, TraceDirs) ->
    wmtracer_setup(TracerNode, TraceDirs, 1).

wmtracer_setup(_TracerNode, [], _N) ->
    ok;
wmtracer_setup(TracerNode, [TraceDir|Rest], N) when is_integer(N), N > 0 ->
    Path = "/dev/wmtrace/" ++ integer_to_list(N),
    wmtracer_setup_one(TracerNode, Path, TraceDir),
    wmtracer_setup(TracerNode, Rest, N + 1).

wmtracer_setup_one(TracerNode, DispatchPath, TraceDir) ->
    DispatchRule = [DispatchPath, TraceDir],
    Res = rpc:call(TracerNode, wmtrace_resource, add_dispatch_rule,
                   DispatchRule, 1000), % Don't wait for long
    case Res of
        ok ->
            ct:log("Added tracer dispatch rule ~p", [DispatchRule]);
        Err ->
            ct:log("Error adding tracer dispatch rule: ~p", [Err])
    end,
    ok.


locate_node(Node0) ->
    case net_adm:ping(Node0) of
        pong ->
            Node0;
        pang ->
            [Name|_] = string:tokens(atom_to_list(Node0), "@"),
            list_to_atom(Name ++ "@" ++ net_adm:localhost())
    end.

