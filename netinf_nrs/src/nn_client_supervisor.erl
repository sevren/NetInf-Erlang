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
%%% @author Jon Borglund
%%% @doc
%%%  This module represents the behaviour of a supervisor process which
%%%  is responsible for the client supervisor processes.
%%% @end
%%% Created : 16 Oct 2012 by Jon Berglund
%%%-------------------------------------------------------------------
-module(nn_client_supervisor).

-behaviour(supervisor).

-export([
	 start_link/0, 
	 start_child/2
	]).

-export([
	 init/1
	]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the cleint supervisor module
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a cild process of the client supervisor, in this case the stream 
%% handler
%% @spec start_child(Args) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------

start_child(stream_handler, Args) ->
   start_c({make_ref(), {nn_stream_handler, start_link, Args}, 
	     temporary, 2000, worker, [nn_event_handler]}).



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
	    nn_logger_server:log(error, ?MODULE, start_c, Err),
	    StartReturn
    end.
		

