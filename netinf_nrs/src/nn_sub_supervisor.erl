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
%%% @author Faroogh Hassan
%%% @author Thomas Nordström
%%% @author Marcus Ihlar
%%% @doc
%%%  This module represents the behaviour of a supervisor process which
%%%  is responsible for the message_handler and event_handler processes.
%%% @end
%%% Created : 16 Oct 2012 by Marcus Ihlar 
%%%-------------------------------------------------------------------
-module(nn_sub_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor process
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Different child processes started by the supervisor.  
%% @end
%%--------------------------------------------------------------------
-spec start_child(atom()) -> {ok, pid()}.
start_child(event_handler) ->
    nn_logger_server:log(verbose, ?MODULE, start_child, 
				  "Starting event_handler process"),
    start_c({make_ref(), {nn_event_handler, start_link, []}, 
	     temporary, 2000, worker, [nn_event_handler]});
start_child({message_handler, Module}) ->
    nn_logger_server:log(verbose, ?MODULE, start_child, 
				  "Starting message_handler process"),
    start_c({make_ref(), {nn_message_handler, start_link, [Module]},
	     temporary, 2000, worker, [nn_message_handler]});
start_child(content_handler) ->
    nn_logger_server:log(verbose, ?MODULE, start_child,
			 "Starting content_handler process"),
    start_c({make_ref(), {nn_content_handler, start_link, []},
	     temporary, 2000, worker, [nn_content_handler]});
start_child(http_forwarder) ->
    nn_logger_server:log(verbose, ?MODULE, start_child,
			 "Starting http_forwarder process"),
    start_c({make_ref(), {nn_http_forwarder, start_link, []},
	     temporary, 2000, worker, [nn_http_forwarder]});
start_child(udp_forwarder) ->
    nn_logger_server:log(verbose, ?MODULE, start_child,
			 "Starting udp_forwarder process"),
    start_c({make_ref(), {nn_udp_forwarder, start_link, []},
	     temporary, 2000, worker, [nn_udp_forwarder]});
start_child(ct_handler) ->
    nn_logger_server:log(verbose, ?MODULE, start_child,
			 "Starting content_transfer_handler process"),
    start_c({make_ref(), {nn_ct_handler, start_link, []},
	     temporary, 2000, worker, [nn_ct_handler]}).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, []}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

start_c(ChildSpec) ->
    case StartReturn = supervisor:start_child(?SERVER, ChildSpec) of
	{ok, _} ->
	    StartReturn;
	{error, Err} ->
	    nn_logger_server:log(error, ?MODULE, start_child, Err),
	    StartReturn
    end.
		

