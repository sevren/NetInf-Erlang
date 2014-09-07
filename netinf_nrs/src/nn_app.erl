%%%-------------------------------------------------------------------
%%% @copyright (C) 2013 Ericsson, Uppsala University
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
%%% 
%%% Uppsala University
%%% 
%%% Project CS course, Fall 2012
%%%
%%% Projekt DV/Project CS, is a course in which the students develop software for
%%% distributed systems. The aim of the course is to give insights into how a big
%%% project is run (from planning to realization), how to construct a complex
%%% distributed system and to give hands-on experience on modern construction
%%% principles and programming methods.
%%%
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Marcus Ihlar
%%% @doc
%%%  Starting point of the nn application. Initiates an http listener
%%%  and starts the main supervisor.
%%% @end
%%% Created : 18 Oct 2012 by Marcus Ihlar 
%%%-------------------------------------------------------------------

-module(nn_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

			 
%%--------------------------------------------------------------------
%% @doc
%% Starts the HTTP Cowboy listener. It also Reads the environmental 
%% variables from the config file and sets up the database and 
%% convergegence layers.
%%
%% @spec start(Type, Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Dispatch = [{'_', [
		       {'_', nn_http_handler, []}
		      ]}
	       ],
    ClientDispatch = [{'_', [
			     {'_', nn_http_client_handler, []}
			    ]}
		     ],
    ContentDispatch = [{'_', [
			     {'_', nn_http_ct_handler, []}
			    ]}
		     ],
    {ok, NrsPort} = application:get_env(netinf_nrs, nrs_port),
    {ok, CtPort} = application:get_env(netinf_nrs, ct_port),
    {ok, ClientPort} = application:get_env(netinf_nrs, client_port),
    {ok, _} = cowboy:start_http(
		http, 100, [{port, NrsPort}],  [{dispatch, Dispatch}]),
    {ok, _} = cowboy:start_http(
		http1, 100, [{port, ClientPort}], [{dispatch, ClientDispatch}]),
    {ok, _} = cowboy:start_http(
		http2, 100, [{port, CtPort}], [{dispatch, ContentDispatch}]),
    ReturnVal = nn_sup:start_link(),
    nn_msgids:init(),
    {ok, LogLevel} = application:get_env(log_level),
    nn_logger_server:add_handler(nn_log_handler, LogLevel),
    {ok, Database} = application:get_env(netinf_nrs, database),
    {ok, Convergence_layers} = application:get_env(netinf_nrs,
						convergence_layers),
    nn_storage:set_db(Database),
    nn_discovery_service:set_cl(Convergence_layers),
    case application:get_env(netinf_nrs, discovery) of
	{ok, off} ->
	    ok;
	_ ->
	    {ok, TimerSendIp}=application:get_env(netinf_nrs, ip_timer),
	    timer:start(),
	    timer:apply_interval(TimerSendIp, nn_discovery_client, send_ip, [])
    end,
    ReturnVal.
    

%%--------------------------------------------------------------------
%% @doc
%% Stops the cowbow listener 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(http),
    cowboy:stop_listener(http1),
    cowboy:stop_listener(http2).
