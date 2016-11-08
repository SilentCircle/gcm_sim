%%% ==========================================================================
%%% Copyright 2013-2016 Silent Circle
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% ==========================================================================

%% @author Edwin Fine <efine@silentcircle.com>
%% @copyright 2013-2016 Silent Circle, LLC.
%% @doc GCM Simulator webmachine_resource.

-module(gcm_sim_resource).
-author('Edwin Fine <efine@silentcircle.com>').

%% Webmachine callback exports
-export([
        init/1,
        allowed_methods/2,
        content_types_provided/2,
        content_types_accepted/2,
        post_is_create/2,
        process_post/2,
        to_html/2,
        to_text/2,
        malformed_request/2,
        finish_request/2
    ]).

%% Functions advertised in content_types_(provided|accepted)
-export([
        to_json/2,
        simulate_gcm_resp/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("lager/include/lager.hrl").
-include_lib("sc_util/include/sc_util_assert.hrl").

-define(SC_DEBUG_LOG(IsDebug, Fmt, Args),
    (IsDebug andalso make_true(lager:info(Fmt, Args)))).

-define(SC_ERROR_LOG(Fmt, Args), lager:error(Fmt, Args)).

-define(DEFAULT_RETRY_TIME, 10).
-define(DEFAULT_STATUS_CODE, 200).

-define(SECONDS_TO_SECS(S), (S)).
-define(MINUTES_TO_SECS(M), (M * ?SECONDS_TO_SECS(60))).
-define(HOURS_TO_SECS(H), (H * ?MINUTES_TO_SECS(60))).
-define(DAYS_TO_SECS(D), (D * ?HOURS_TO_SECS(24))).
-define(WEEKS_TO_SECS(W), (W * ?DAYS_TO_SECS(7))).

-define(dbg_msg(Fmt, Args),
        lager:info("[~s:~B] " ++ Fmt,
                   [filename:basename(?FILE), ?LINE] ++ Args)).

-record(sim_ctx, {
        status_code = ?DEFAULT_STATUS_CODE,
        results = [],
        retry_after = ?DEFAULT_RETRY_TIME,
        json = []
    }).

-record(ctx, {
        cfg = [],
        req = [],
        rsp = [],
        sim_ctx = #sim_ctx{}
    }).

-type wrq() :: #wm_reqdata{}.
-type werr() :: {error, iolist()}.
-type wret(T) :: {T | werr(), wrq(), ctx()}.
-type wbool_ret() :: wret(boolean()).
-type wiolist_ret() :: wret(iolist()).

-type ctx() :: #ctx{}.
-type sim_ctx() :: #sim_ctx{}.

-type pl(KT, VT) :: list({KT, VT}).
-type config() :: pl(atom(), term()).
-type filepath() :: string().
-type debug_info() :: ok | {trace, filepath()}.

-type json() :: binary().

%% JSON as decoded from JSON string by jsx
-type unicode() :: binary().
-type ejson() :: ejson_term().
-type ejson_term() :: ejson_scalar() | ejson_collection().
-type ejson_scalar() :: boolean() | 'null' | number() | unicode().
-type ejson_collection() :: ejson_list() | ejson_dict().
-type ejson_list() :: list(ejson_term()).
-type ejson_dict() :: ejson_empty_dict() | list(ejson_field()).
-type ejson_empty_dict() :: [{}].
-type ejson_field() :: {ejson_field_name(), ejson_term()}.
-type ejson_field_name() :: atom() | unicode().

%%====================================================================
%% Webmachine request callback functions
%%====================================================================
%% Note: Config comes from dispatch.conf, NOT environment!
-spec init(config()) -> {debug_info(), ctx()}.
init(Config) ->
    {config_debug(Config), #ctx{cfg = Config}}.

-spec config_debug(config()) -> debug_info().
config_debug(Config) ->
    case pv(debug, Config, false) of
        false ->
            ok;
        true ->
            ok;
        [_|_] = DbgFilePath ->
            {trace, DbgFilePath}
    end.

-spec finish_request(wrq(), ctx()) -> wbool_ret().
finish_request(ReqData, Context) ->
    Headers = [
            {"Cache-Control", "must-revalidate,no-cache,no-store"}
            ],
    NewReqData = wrq:set_resp_headers(enc_headers(Headers), ReqData),
    {true, NewReqData, Context}.

-spec allowed_methods(wrq(), ctx()) -> {list(), wrq(), ctx()}.
allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

-spec content_types_provided(wrq(), ctx()) -> {list(), wrq(), ctx()}.
content_types_provided(ReqData, Context) ->
   {
       [
           {"application/json", to_json}
       ],
       ReqData, Context
   }.

-spec content_types_accepted(wrq(), ctx()) -> {list(), wrq(), ctx()}.
content_types_accepted(ReqData, Context) ->
    {
        [
            {"application/json", simulate_gcm_resp}
        ],
       ReqData, Context
    }.

-spec post_is_create(wrq(), ctx()) -> wbool_ret().
post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

-spec malformed_request(wrq(), ctx()) -> wbool_ret().
malformed_request(ReqData, Ctx) ->
    case wrq:req_body(ReqData) of
        <<Body/binary>> ->
            check_if_malformed_req(ReqData, Ctx, Body);
        undefined ->
            Err = "MissingRegistration",
            {true, malformed_err(Err, ReqData), Ctx}
    end.

-spec process_post(wrq(), ctx()) -> wbool_ret().
process_post(ReqData, Context) ->
    Debug = is_debug(ReqData, Context),
    ?SC_DEBUG_LOG(Debug, "~p:process_post called", [?MODULE]),
    simulate_gcm_resp(ReqData, Context).

%% @private
%% @doc Just uses the same req body that should already be JSON
-spec to_json(wrq(), ctx()) -> {[json()], wrq(), ctx()}.
to_json(ReqData, Context) ->
    Debug = is_debug(ReqData, Context),
    ?SC_DEBUG_LOG(Debug, "~p:to_json called", [?MODULE]),
    JSON = jsx:encode(Context#ctx.rsp),
    {[JSON], ReqData, Context}.

-spec to_html(wrq(), ctx()) -> wiolist_ret().
to_html(ReqData, Context) ->
    {"<html><body>Hello, new world</body></html>", ReqData, Context}.

%%====================================================================
%%% Helper functions
%%%%====================================================================
-spec check_if_malformed_req(wrq(), #ctx{}, binary()) ->
    {boolean(), wrq(), #ctx{}}.
check_if_malformed_req(ReqData, Ctx, Body) ->
    case validate_req(Body) of
        {ok, ReqProps} ->
            {false, ReqData, Ctx#ctx{req = ReqProps}};
        {error, Reason} ->
            lager:error("Malformed request, reason: ~s", [Reason]),
            {true, malformed_err(Reason, ReqData), Ctx}
    end.

-spec malformed_err(string(), wrq()) -> wrq().
malformed_err(Error, ReqData) when is_list(Error) ->
    RD = wrq:set_resp_body(Error, ReqData),
    wrq:set_resp_header("Content-Type", "text/plain; charset=UTF-8", RD).

-spec validate_req(binary()) -> {ok, list()} | {error, string()}.
validate_req(Body) ->
    try
        EJSON = parse_json(Body),
        validate_gcm_req(EJSON)
    catch
        throw:{_ExcName, Reason} when is_list(Reason) ->
            {error, Reason}
    end.

-spec simulate_gcm_resp(wrq(), ctx()) -> wbool_ret().
simulate_gcm_resp(ReqData, Context) ->
    Debug = is_debug(ReqData, Context),
    ?SC_DEBUG_LOG(Debug, "~p:simulate_gcm_resp/2 ReqData:~n~p~nContext:~n~p",
                  [?MODULE, ReqData, Context]),

    RetVal = try_req(fun process_gcm_req/2, ReqData, Context),

    ?SC_DEBUG_LOG(Debug, "~p:simulate_gcm_resp/2 returned:~n~p", [?MODULE, RetVal]),

    RetVal.

-spec to_text(wrq(), ctx()) -> wiolist_ret().
to_text(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    Body = io_lib:format("Hello ~s from Webmachine.~n", [Path]),
    {Body, ReqData, Context}.

-spec try_req(fun((wrq(), ctx()) -> wbool_ret()), wrq(), ctx())
        -> wbool_ret().
try_req(Fun, ReqData, Context) ->
    try
        Fun(ReqData, Context)
    catch
        throw:{_ExcName, Text} when is_list(Text) ->
            RD = wrq:set_resp_header("Content-Type", "text/plain; charset=UTF-8",
                                     wrq:set_resp_body(Text, ReqData)),
            {{halt, 400}, RD, Context};
        throw:{sim_json, 200, JSON} when is_list(JSON) ->
            {{halt, 200}, wrq:set_resp_body(JSON, ReqData), Context};
        throw:{_ExcName, Status, Text} when is_integer(Status), is_list(Text) ->
            RD = wrq:set_resp_header("Content-Type", "text/plain; charset=UTF-8",
                                     wrq:set_resp_body(Text, ReqData)),
            {{halt, Status}, RD, Context};
        _:Reason ->
            lager:error("Unhandled exception: ~p~n~p",
                        [Reason, erlang:get_stacktrace()]),
            {{error, <<"Internal Server Error">>}, ReqData, Context}
    end.

-spec process_gcm_req(wrq(), ctx()) -> wbool_ret().
process_gcm_req(ReqData, #ctx{} = Context) ->
    SimCtx = make_sim_ctx(ReqData, Context),
    GCMRespEJSON = process_valid_req(SimCtx),
    EJSON = maybe_add_sim_ctx(SimCtx, GCMRespEJSON, ReqData, Context),
    JSON = jsx:encode(EJSON),
    NewContext = Context#ctx{rsp = GCMRespEJSON, sim_ctx = SimCtx},
    RD = maybe_set_retry_after(SimCtx#sim_ctx.retry_after,
                               GCMRespEJSON, ReqData),
    {true, wrq:set_resp_body(JSON, RD), NewContext}.

make_sim_ctx(ReqData, #ctx{req = ReqProps} = Context) ->
    Data = pv(<<"data">>, ReqProps, []),
    case pv(<<"sim_cfg">>, Data, []) of
        [] ->
            make_sim_ctx_from_headers(ReqData, Context);
        [{_,_}|_] = SimCfg ->
            make_sim_ctx_from_sim_cfg(SimCfg)
    end.

make_sim_ctx_from_headers(ReqData, Context) ->
    StatusCode = maybe_halt_on_status_code(sim_status_code(ReqData, Context)),
    RetryAfter = sim_retry_after(ReqData, Context),
    JSONOverride = maybe_return_json(sim_json(ReqData, Context)),
    #sim_ctx{
        status_code = StatusCode,
        results = sim_results(ReqData, Context),
        retry_after = RetryAfter,
        json = JSONOverride
    }.

%%--------------------------------------------------------------------
% Precedence is:
%
% - body trumps reason
% - status_code must be provided if body is provided
% - status_code overrides reason
%
% The valid combinations are:
%
% - status_code and body
% - reason
% - status_code and reason
%
%%--------------------------------------------------------------------
make_sim_ctx_from_sim_cfg(SimCfg) ->
    StatusCode = sim_status_code(SimCfg),
    Results = sim_results(SimCfg),
    Body = sim_body(SimCfg),
    Delay = sim_delay(SimCfg),

    ?dbg_msg("StatusCode = ~p\n", [StatusCode]),
    ?dbg_msg("Results = ~p\n", [Results]),
    ?dbg_msg("Body = ~p\n", [Body]),
    ?dbg_msg("Delay = ~p\n", [Delay]),

    %% result on input is a string in the same format as
    %% the "X-GCMSimulator-Results" header.
    %%
    %% results in #sim_ctx{} must be a list of proplists.
    %% Each proplist corresponds to one result.
    %% The proplist contains a combination of the following:
    %%
    %% {message_id, string()}
    %% {registration_id, string()}
    %% {error, string()}
    %%
    %% The valid combinations are:
    %%
    %% [{error, _}] - Operation failed
    %% [{message_id, _}] - Operation succeeded
    %% [{message_id, _}, {registration_id, _}] - Failed, use canonical id
    %%
    #sim_ctx{
        status_code = StatusCode,
        results = Results,
        retry_after = Delay,
        json = Body
    }.

sim_body([{_,_}|_] = Cfg) ->
    base64:decode(pv(<<"body">>, Cfg, <<>>)).

sim_delay([{_,_}|_] = Cfg) ->
    pv(<<"delay">>, Cfg, 0).

sim_status_code([{_,_}|_] = Cfg) ->
    list_to_integer(to_s(pv(<<"status_code">>, Cfg, <<"200">>))).

%% @doc Return the status code provided by header.
%% Default to 200.
-spec sim_status_code(wrq(), ctx()) -> integer().
sim_status_code(ReqData, _Context) ->
    hdr_to_integer("X-GCMSimulator-StatusCode", ReqData, 200).

-spec sim_results(proplists:proplist()) -> ejson().
sim_results([{_,_}|_] = Cfg) ->
    case pv(<<"results">>, Cfg) of
        undefined ->
            ?throwExcMsg(sim_results_required,
                         "'results' property is required", []);
        Results ->
            parse_sim_results(sc_util:to_list(Results))
    end.

%% @doc Return the predefined results provided by header,
%% parsed into EJSON.
-spec sim_results(wrq(), ctx()) -> ejson().
sim_results(ReqData, _Context) ->
    case get_req_hdr(H = "X-GCMSimulator-Results", ReqData, []) of
        [_|_] = Str ->
            parse_sim_results(Str);
        [] ->
            ?throwExcMsg(sim_results_required,
                         "HTTP header ~p is required",
                         [H])
    end.

-spec sim_retry_after(wrq(), ctx()) -> integer().
sim_retry_after(ReqData, _Context) ->
    hdr_to_integer("X-GCMSimulator-Retry-After", ReqData, ?DEFAULT_RETRY_TIME).

-spec sim_json(wrq(), ctx()) -> ejson() | undefined.
sim_json(ReqData, _Context) ->
    case get_req_hdr("X-GCMSimulator-JSON", ReqData) of
        undefined ->
            undefined;
        JSON ->
            parse_json(to_binary(JSON))
    end.

%%@doc Parse a sim results header. The header format is as follows:
%% ```
%%  Header ::= NameValuePairs OptHeader | <empty>
%%  OptHeader ::= ';' Header | <empty>
%%  NameValuePairs ::= NameValuePair OptNameValuePairs
%%  OptNameValuePairs ::= ',' NameValuePair | <empty>
%%  NameValuePair ::= Name ':' Value
%%  Name ::= message_id | registration_id | error
%%  '''
-spec parse_sim_results(string()) -> [pl(atom(), unicode())].
parse_sim_results(Str) ->
    PairsList = string:tokens(Str, ";"),
    [parse_pairs(string:tokens(Pairs, ",")) || Pairs <- PairsList].

-spec parse_pairs(list(string())) -> pl(atom(), unicode()).
parse_pairs(Pairs) ->
    [parse_pair(Pair) || Pair <- Pairs].

-spec parse_pair(string()) -> {atom(), unicode()}.
parse_pair(Pair) ->
    [Name, Val] = try
        [_, _] = split_at(Pair, ":")
    catch
        _:_ ->
            invalid_results_hdr()
    end,
    make_pair(Name, Val).

-spec make_pair(string(), string()) -> {atom(), unicode()}.
make_pair(Name, Val) ->
    Unicode = unicode:characters_to_binary(Val, utf8),
    ?assertExcMsg(is_binary(Unicode), bad_utf8, "Bad utf8: ~w", [Val]),
    {parse_name(Name), Unicode}.

-spec parse_name(string()) -> atom().
parse_name("message_id") -> message_id;
parse_name("registration_id") -> registration_id;
parse_name("error") -> error;
parse_name(_Invalid) -> invalid_results_hdr().

invalid_results_hdr() ->
    ?throwExcMsg(invalid_results_header, "SimInvalidResultsHdr", []).

%% @doc An example of an actual GCM response is
%% ```
%% {"multicast_id":8640031304644822041,
%%  "success":1,
%%  "failure":0,
%%  "canonical_ids":0,
%%  "results":[{"message_id":"0:1358221350928814%921c249af9fd7ecd"}]}
%% '''
%% Here's another, this time an error:
%% ```
%%  {
%%      "multicast_id": 6378477543311341717,
%%      "success": 0
%%      "failure": 1,
%%      "canonical_ids": 0,
%%      "results": [
%%          {
%%              "error": "InvalidRegistration"
%%          }
%%      ],
%%  }
%% '''


-spec process_valid_req(sim_ctx()) -> ejson().
process_valid_req(#sim_ctx{results = Results}) ->
    {NumSuccess, NumFailure, NumCanonical} = count_results(Results),
    make_gcm_resp(NumSuccess, NumFailure, NumCanonical, Results).

make_gcm_resp(NumSuccess, NumFailure, NumCanonicals, Results) when
        is_integer(NumSuccess),
        is_integer(NumFailure),
        is_integer(NumCanonicals),
        is_list(Results) ->
    [{multicast_id, make_multicast_id()},
     {success, NumSuccess},
     {failure, NumFailure},
     {canonical_ids, NumCanonicals},
     {results, Results}].

make_multicast_id() ->
    % large integer
    8640000000000000000 + random:uniform(100000000000000).

count_matching_results(MatchFun, GCMRespJSON) when is_function(MatchFun, 1) ->
    case pv(results, GCMRespJSON) of
        [[{_,_}|_]|_] = ListOfPLs ->
            lists:foldl(
                fun(PL, Acc) ->
                        case MatchFun(PL) of
                            true -> 1;
                            false -> 0
                        end + Acc
                end, 0, ListOfPLs);
        _ ->
            0
    end.

count_results(Results) ->
    lists:foldl(
        fun(PL, {Sa,Fa,Ea}) ->
                {S,F,E} = categorize(PL),
                {S + Sa, F + Fa, E + Ea}
        end,
        {0, 0, 0},
        Results).

-spec categorize(Result::pl(atom(), unicode())) ->
    {Success::integer(), Failure::integer(), Canonical::integer()}.
categorize(R) ->
    case {pv(message_id, R), pv(registration_id, R), pv(error, R)} of
        {undefined, undefined, Err} when Err /= undefined ->
            {0, 1, 0};
        {_, undefined, undefined} ->
            {1, 0, 0};
        {_, _, undefined} ->
            {1, 0, 1};
        {S, F, E} ->
            ?throwExcMsg(invalid_results, "{success:~w,failure:~w,error:~w}",
                         [S, F, E])
    end.

-spec sim_ctx_to_ejson(sim_ctx()) -> ejson().
sim_ctx_to_ejson(SimCtx) ->
    #sim_ctx{status_code = StatusCode,
             results = Results,
             retry_after = RetryAfter,
             json = JSON} = SimCtx,
    [
        {<<"status_code">>, StatusCode},
        {<<"results">>, Results},
        {<<"retry_after">>, RetryAfter},
        {<<"json">>, JSON}
    ].

%% Check status code. If it's not a 2xx, don't bother
%% with anything else, just return the status.
maybe_halt_on_status_code(StatusCode) ->
    if
        StatusCode >= 200, StatusCode =< 299 ->
            StatusCode;
        true ->
            throw({sim_status_code,
                   StatusCode,
                   "Stopped by X-GCMSimulator-StatusCode"})
    end.

%% If the header was provided, we want to return the given JSON.
maybe_return_json(undefined) ->
    [{}];
maybe_return_json(JSON) ->
    throw({sim_json, 200, JSON}).

is_unavailable(PL) ->
    pv(error, PL) =:= <<"Unavailable">>.

maybe_set_retry_after(RetryAfter, GCMRespJSON, ReqData) ->
    case count_matching_results(fun is_unavailable/1, GCMRespJSON) of
        0 -> % Omit Retry-After header
            ReqData;
        _ ->
            wrq:set_resp_header("Retry-After", to_s(RetryAfter), ReqData)
    end.

maybe_add_sim_ctx(SimCtx, EJSON, ReqData, Context) ->
    case is_debug(ReqData, Context) of
        true  ->
            [{<<"sim_ctx">>, sim_ctx_to_ejson(SimCtx)} | EJSON];
        false ->
            EJSON
    end.

enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [{atom_to_list(Tag), enc_header_val(Val)} | enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [{Tag, enc_header_val(Val)} | enc_headers(T)];
enc_headers([]) ->
    [].

enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.

pv(K, PL) ->
    proplists:get_value(K, PL).

pv(K, PL, Def) ->
    proplists:get_value(K, PL, Def).

pvs(KeysAndDefs, PL) ->
    [{Key, pv(Key, PL, Def)} || {Key, Def} <- KeysAndDefs].

-compile({inline, [{make_true, 1}]}).
make_true(_X) -> true.

-spec to_binary(term()) -> binary().
to_binary(<<X/binary>>) ->
    X;
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_integer(X) ->
    to_binary(integer_to_list(X));
to_binary(X) ->
    to_binary(lists:flatten(io_lib:format("~w", [X]))).

to_s(N) when is_integer(N) ->
    integer_to_list(N);
to_s(L) when is_list(L) ->
    L;
to_s(<<B/binary>>) ->
    binary_to_list(B).

-spec hdr_to_integer(string(), wrq(), integer()) -> integer().
hdr_to_integer(HeaderName, ReqData, Default) when is_integer(Default) ->
    case get_req_hdr(HeaderName, ReqData) of
        undefined ->
            Default;
        Str ->
            list_to_integer(Str)
    end.

-spec is_debug(wrq(), ctx()) -> boolean().
is_debug(ReqData, #ctx{cfg = Config}) ->
    get_req_hdr("X-GCMSimulator-Debug", ReqData, "false") =:= "true" orelse
    pv(debug, Config, false) =:= true.

-spec get_req_hdr(string(), wrq()) -> string() | 'undefined'.
get_req_hdr(HeaderName, ReqData) ->
    wrq:get_req_header(string:to_lower(HeaderName), ReqData).

-spec get_req_hdr(string(), wrq(), string()) -> string().
get_req_hdr(HeaderName, ReqData, Default) when is_list(Default) ->
    case get_req_hdr(HeaderName, ReqData) of
        undefined ->
            Default;
        Val ->
            Val
    end.


%%--------------------------------------------------------------------
%% @doc Parse a GCM JSON notification and return it as a property list.
%%
%% To understand the various properties below, please see
%% <a href="http://developer.android.com/guide/google/gcm/index.html">
%% GCM Architectural Overview</a>.
%% The description given is basically to show how to format the
%% properties in Erlang.
%%
%% == Notification Properties ==
%%
%% <dl>
%%   <dt>`to::binary()'</dt>
%%      <dd>A binary string, which is a registration id
%%      for an Android device+application. <strong>Required</strong>
%%      unless `registration_ids' is provided. It is an error to provide
%%      both `registration_ids' and this key.
%%      </dd>
%%   <dt>`registration_ids::[binary()]'</dt>
%%      <dd>List of binary strings. Each binary is a registration id
%%      for an Android device+application. <strong>Required</strong>
%%      unless `to' is provided. It is an error to provide both `to'
%%      and this key.
%%      </dd>
%%   <dt>`collapse_key::binary() '</dt>
%%      <dd>Binary string. Optional.</dd>
%%   <dt>`delay_while_idle::boolean()'</dt>
%%      <dd>See GCM reference. Optional.</dd>
%%   <dt>`time_to_live::integer()'</dt>
%%      <dd>Optional.</dd>
%%   <dt>`restricted_package_name::binary()'</dt>
%%      <dd>Binary string - overrides default on server. Optional.</dd>
%%   <dt>`dry_run::boolean()'</dt>
%%      <dd>Optional (defaults to false)</dd>
%%   <dt>`data::[{binary(), any()}]'</dt>
%%      <dd>Message payload data, which must be an
%%          object (Erlang proplist) as described in the table below.
%%
%%      <table class="with-borders">
%%        <tr>
%%          <th><strong>json</strong></th><th><strong>erlang</strong></th>
%%        </tr>
%%        <tr>
%%          <td> <code>number</code> </td>
%%          <td> <code>integer()</code> and <code>float()</code></td>
%%        </tr>
%%        <tr>
%%          <td> <code>string</code> </td>
%%          <td> <code>binary()</code> </td>
%%        </tr>
%%        <tr>
%%          <td> <code>true</code>, <code>false</code> and <code>null</code></td>
%%          <td> <code>true</code>, <code>false</code> and <code>null</code></td>
%%        </tr>
%%        <tr>
%%          <td> <code>array</code> </td>
%%          <td> <code>[]</code> and <code>[JSON]</code></td>
%%        </tr>
%%        <tr>
%%          <td> <code>object</code> </td>
%%          <td> <code>[{}]</code> and <code>[{binary() OR atom(), JSON}]</code></td>
%%        </tr>
%%      </table>
%%      </dd>
%% </dl>
%%
%% == Notification with single registration id ==
%%
%% ```
%% Notification = [
%%     {'to', <<"Your android app reg id">>},
%%     {'data', [{<<"alert">>, <<"Hello, world!">>}]}
%% ].
%% '''
%%
%% == Notification using list of registration ids ==
%%
%% ```
%% RegIds = [<<"Android app reg id 1">>, <<"Android app reg id 2">>],
%% Notification = [
%%     {'registration_ids', RegIds},
%%     {'data', [{<<"alert">>, <<"Hello, world!">>}]}
%% ].
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_json(json()) -> ejson().
parse_json(<<JSON/binary>>) ->
    Result = try jsx:decode(JSON) of
        EJ when is_list(EJ) ->
            {ok, EJ};
        _ ->
            {error, []}
    catch
        _:_ ->
            {error, []}
    end,

    case Result of
        {ok, EJSON} ->
            EJSON;
        {error, _} ->
            ?throwExcMsg(bad_json, "JSON_PARSING_ERROR: Invalid JSON", [])
    end.

%%--------------------------------------------------------------------
-spec validate_gcm_req(ejson()) -> {ok, ejson_dict()}.
validate_gcm_req(EJSON) ->
    {ok, [validate_reg_ids(EJSON) |
          validate_rest(delete_props([<<"to">>, <<"registration_ids">>],
                                     EJSON))]}.

%%--------------------------------------------------------------------
validate_reg_ids(EJSON) ->
    To = pv(<<"to">>, EJSON),
    RegIds = pv(<<"registration_ids">>, EJSON),
    case {To, RegIds} of
        {undefined, undefined} ->
            ?throwExcMsg(regids_missing,
                         "Either 'to' or 'registration_ids' expected",
                         []);
        _ when To /= undefined andalso RegIds /= undefined ->
            ?throwExcMsg(regids_conflict,
                         "Cannot provide both 'to' and 'registration_ids'",
                         []);
        {_, undefined} ->
            validate_gcm_field(Field = {<<"to">>, To}),
            Field;
        {undefined, _} ->
            validate_gcm_field(Field = {<<"registration_ids">>, RegIds}),
            Field
    end.

%%--------------------------------------------------------------------
validate_rest(EJSON) ->
    KeysAndDefs = [
        {<<"collapse_key">>, undefined},
        {<<"delay_while_idle">>, false},
        {<<"time_to_live">>, ?WEEKS_TO_SECS(4)},
        {<<"restricted_package_name">>, undefined},
        {<<"dry_run">>, false},
        {<<"data">>, []}
    ],
    Props = pvs(KeysAndDefs, EJSON),
    [validate_gcm_field(KV) || KV <- Props],
    [{K, V} || {K, V} <- Props, V =/= undefined].

%%--------------------------------------------------------------------
-spec validate_gcm_field(ejson_field()) -> true.
validate_gcm_field({<<"to">> = Name, Field}) ->
    ?assertNonEmptyStr(Name, Field);
validate_gcm_field({<<"registration_ids">> = B, []}) ->
    Name = binary_to_list(B),
    ?throwExcMsg(regids_empty, "~p expected to be a non-empty list", [Name]);
validate_gcm_field({<<"registration_ids">> = B, RegIds}) ->
    Name = binary_to_list(B),
    ?assertExcMsg(is_list(RegIds), expected_list,
                  "~p expected to be a list", [Name]),
    ?assertExcMsg(lists:all(fun is_non_empty_bin/1, RegIds), empty_reg_id,
                  "~p contains badly formed registration id", [Name]);
validate_gcm_field({<<"collapse_key">>, undefined}) ->
    true;
validate_gcm_field({<<"collapse_key">> = Name, Field}) ->
    ?assertNonEmptyStr(Name, Field);
validate_gcm_field({<<"delay_while_idle">> = Name, Field}) ->
    ?assertBoolean(Name, Field);
validate_gcm_field({<<"time_to_live">> = Name, Field}) ->
    ?assertIntegerInRange(Name, Field, 0, ?WEEKS_TO_SECS(4));
validate_gcm_field({<<"restricted_package_name">>, undefined}) ->
    true;
validate_gcm_field({<<"restricted_package_name">> = Name, Field}) ->
    ?assertNonEmptyStr(Name, Field);
validate_gcm_field({<<"dry_run">> = Name, Field}) ->
    ?assertBoolean(Name, Field);
validate_gcm_field({<<"data">>, undefined}) ->
    true;
validate_gcm_field({<<"data">>, Field}) ->
    ?assert(is_list(Field)),
    L = [?assert(is_binary(K)) || {K, _} <- Field],
    ?assert(length(L) == length(Field)); % All data are {K,V} pairs
validate_gcm_field({<<Other/binary>>, Field}) ->
    ?throwMsg("Unsupported field ~s = ~p", [binary_to_list(Other), Field]).

%%--------------------------------------------------------------------
-spec split_at(String, Separator) -> StringList when
      String :: string(), Separator :: string(),
      StringList :: [string()].
split_at(String, Separator) ->
    case string:str(String, Separator) of
        0 ->
            [String];
        Pos ->
            [string:left(String, Pos - 1), string:substr(String, Pos + 1)]
    end.

%%--------------------------------------------------------------------
delete_props(Keys, Props) ->
    lists:foldl(fun(K, Acc) -> lists:keydelete(K, 1, Acc) end,
                Props, Keys).

