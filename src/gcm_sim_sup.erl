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
%% @copyright 2013 Silent Circle, LLC.

%% @doc Supervisor for the gcm_sim application.

-module(gcm_sim_sup).
-author('Edwin Fine <efine@silentcircle.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_link/1, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("lager/include/lager.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    start_link([]).

%% @spec start_link(Env) -> ServerRet
%% @doc API for starting the supervisor.
start_link(Env) when is_list(Env) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Env).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    DefaultEnv = default_env(),
    [_|_] = DefaultEnv, % Assert not empty env
    init([{wm_config, DefaultEnv}]);
init([_|_] = Env) ->
    WmConfig0 = proplists:get_value(wm_config, Env, default_env()),
    Dispatch = get_dispatch_list(WmConfig0),
    WmConfig = lists:keystore(dispatch, 1, WmConfig0, {dispatch, Dispatch}),
    log_config(WmConfig),
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WmConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.

default_env() ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,

    Port = case os:getenv("WEBMACHINE_PORT") of
            false -> 8000;
            AnyPort -> AnyPort
          end,
    Dispatch = get_dispatch_list([]),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, Dispatch}
    ].

get_dispatch_list(Env) ->
    {ok, App} = application:get_application(?MODULE),
    PrivDir = priv_dir(App),
    lager:info("PrivDir = ~s", [PrivDir]),

    Dispatch = proplists:get_value(dispatch, Env, "dispatch.conf"),

    case find_file([Dispatch, {PrivDir, Dispatch}]) of
        {ok, File} ->
            {ok, DispatchList} = file:consult(File),
            DispatchList;
        false ->
            Dispatch % Hopefully is dispatch list
    end.

find_file([{Base, Name}|Rest]) ->
    try filename:join(Base, Name) of
        Joined ->
            find_file([Joined|Rest])
    catch
        _:_ ->
            find_file(Rest)
    end;
find_file([File|Rest]) ->
    case filelib:is_file(File) of
        true ->
            {ok, File};
        false ->
            find_file(Rest)
    end;
find_file([]) ->
    false.

%% @doc return the priv dir
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            lager:info("code:priv_dir(~p) returned {error,bad_name}, using ebin dir ~s", [Mod, Ebin]),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            lager:info("code:priv_dir(~p) returned ~s", [Mod, PrivDir]),
            PrivDir
    end.

log_config(WmConfig) ->
    Fmt = fun({K, V}, Acc) -> [io_lib:format("~p = ~p~n", [K, V]) | Acc] end,
    ConfigInfo = lists:flatten(lists:foldl(Fmt, [], WmConfig)),
    lager:info("%%% WebMachine config %%%~n~s~n", [ConfigInfo]).

