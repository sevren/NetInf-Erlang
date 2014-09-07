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
%%% @doc
%%% This module handles the incoming events requests
%%% @end
%%% Created : 19 Oct 2012 by Kiril Goguev Daniele Bacarella
%%%-------------------------------------------------------------------
-module(nn_log_handler).

-behaviour(gen_event).
 
-export([
	 init/1, 
	 handle_event/2, 
	 handle_call/2, 
	 handle_info/2, 
	 code_change/3,
	 terminate/2
	]).

-define(DUMPVAL, 20).

-record(state, {
	  data = [] :: [term()],
	  logs = 0 :: integer(),
	  level = error :: log_level()
	 }).

-opaque log_level() :: error | warning | verbose.

-export_type([log_level/0]). 
 
init([]) ->
    {ok, #state{}};
init(Level) ->
    {ok, #state{level = Level}}.


%%--------------------------------------------------------------------
%%@doc 
%% Call the log function in nn_logger module to actually handle the 
%% event.
%%  PARAMETERS:
%%  - Level: verbose
%%  - Module: the name of the module in which this function is called
%%  - Action: the name of the function in which this log function is called  
%%  - Msg:    the actual log message
%% @end
%%--------------------------------------------------------------------
handle_event({Level, Module, Action, Msg}, State) ->
    StateLevel = State#state.level,
    case is_right_level(Level, StateLevel) of
	true ->
	    Logs = State#state.logs,
	    Log = [
		   binary_to_list(nn_util:create_timestamp()), 
		   Level, 
		   Module, 
		   Action, 
		   Msg
		  ],
	    Data = [Log | State#state.data],
	    case Logs >= ?DUMPVAL of
		true ->
		    nn_logger:log(Data),
		    NewState = State#state{logs = 0, data = []},
		    {ok, NewState} ;
		false ->
		    {ok, State#state{logs = Logs + 1, data = Data}}
	    end;
	false ->
	    {ok, State}
    end;
handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, State) ->
    case State#state.logs of
	0 ->
	    {ok, State};
	_ ->
	    nn_logger:log(State#state.data),
	    {ok, State}
    end.

is_right_level(_, verbose) ->
    true;
is_right_level(Level, warning) when Level =/= verbose ->
    true;
is_right_level(error, error) ->
    true;
is_right_level(_, _) ->
    false.
