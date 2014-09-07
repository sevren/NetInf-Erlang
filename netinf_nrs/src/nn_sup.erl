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
%%%  Main supervisor for the nn application.
%%% @end
%%% Created : 16 Oct 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(nn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, 
			 permanent, infinity, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Subsup = ?CHILD(nn_sub_supervisor, supervisor),
    MsgIdSup = ?CHILD(nn_msgid_sup, supervisor),
    ClientSup = ?CHILD(nn_client_supervisor, supervisor),
    Storage = ?CHILD(nn_storage, worker),
    Discovery = ?CHILD(nn_discovery_service, worker),
    Logger = ?CHILD(nn_logger_server, worker),
    Stats = ?CHILD(nn_stats, worker),
    UDPHandler = ?CHILD(nn_udp_handler, worker),
    Children = [
		Subsup,
		MsgIdSup,
		Storage,
		Logger,
		Discovery,
		Stats,
		UDPHandler,
		ClientSup
	       ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

