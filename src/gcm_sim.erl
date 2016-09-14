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

%% @doc gcm_sim startup code

-module(gcm_sim).
-author('Edwin Fine <efine@silentcircle.com>').
-export([start/0, start/1, start_link/0, start_link/1, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_link([]).

%% @doc Starts the app for inclusion in a supervisor tree.
%%
-spec start_link(list()) -> {ok,Pid::pid()}.
start_link(Env) ->
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    gcm_sim_sup:start_link(Env).

%% @spec start() -> ok
%% @doc Start the gcm_sim server.
start() ->
    start([]).

%% @doc Start the gcm_sim server.

-spec start(Env) -> ok when
      Env :: proplists:proplist().
start(SimEnv) when is_list(SimEnv) ->
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    [ok = application:set_env(gcm_sim, K, V) || {K, V} <- SimEnv],
    {ok, _} = application:ensure_all_started(gcm_sim),
    ok.

%% @spec stop() -> ok
%% @doc Stop the gcm_sim server.
stop() ->
    application:stop(gcm_sim).
