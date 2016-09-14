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

%% @doc Callbacks for the gcm_sim application.

-module(gcm_sim_app).
-author('Edwin Fine <efine@silentcircle.com>').

-behaviour(application).
-export([start/2,stop/1]).

-include_lib("lager/include/lager.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for gcm_sim.
start(_Type, _StartArgs) ->
    {ok, App} = application:get_application(?MODULE),
    Env = application:get_all_env(App),
    lager:info("Env for ~p: ~p", [App, Env]),
    gcm_sim_sup:start_link(Env).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for gcm_sim.
stop(_State) ->
    ok.
