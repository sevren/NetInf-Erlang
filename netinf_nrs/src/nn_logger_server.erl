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
%%% @author Kiril Goguev 
%%% @author Daniele Bacarella
%%% @author Marcus Ihlar
%%% @doc
%%%  This module defines the behaviour of the logger server
%%%  Allows modules to connect to the server using an event handler.
%%%  Send events when a module calls the log function. 
%%% @end
%%% Created : 19 Oct 2012 by Kiril Goguev Daniele Bacarella
%%%-------------------------------------------------------------------
-module(nn_logger_server).
 
-export([
	 start_link/0, 
	 add_handler/2, 
	 log/4
	]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% @doc
%%  Start a link to an instance of gen event
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link()->
    gen_event:start_link({local, ?SERVER}).

%%%-------------------------------------------------------------------    
%% @doc
%%  Adds a handler to the event stream.
%% @end
%%%-------------------------------------------------------------------
-spec add_handler(atom(), term()) -> 
			 ok | {'EXIT', Reason :: term()} | term().
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%%-------------------------------------------------------------------    
%% @doc 
%%  Send an event to log
%%  PARAMETERS:
%%  - Level: verbose
%%  - Module: the name of the module in which this function is called
%%  - Action: the name of the function in which this log function is called  
%%  - Msg:    the actual log message
%% @end
%%%-------------------------------------------------------------------
-spec log(atom(), atom(), atom(), list()) -> ok.
log(Level, Module, Action, Msg)->
    gen_event:notify(?SERVER, {Level, Module, Action, Msg}).

