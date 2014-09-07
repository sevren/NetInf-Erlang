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
%%% @author Thomas Nordström
%%% @doc
%%%  Value container processes, that allow for timed storage.
%%% @end
%%% Created : 19 Nov 2012 by Marcus Ihlar 
%%%-------------------------------------------------------------------
-module(nn_msgide).

-behaviour(gen_server).

%% API Functions
-export([
	 start_link/2,
	 create/2,
	 create/1,
	 fetch/1,
	 delete/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).

%%=============================================================
%% API SECTION
%%=============================================================

%%--------------------------------------------------------------------
%% @doc
%% starts the server 
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), integer() | infinity) -> {ok, pid()}.
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%%--------------------------------------------------------------------
%% @doc
%%  Creates a new element process with a specified timeout value.
%% @end
%%--------------------------------------------------------------------
-spec create(pid(), integer() | infinity) -> supervisor:startchild_ret().
create(Value, LeaseTime) ->
    nn_msgid_sup:start_child(Value, LeaseTime).

%%--------------------------------------------------------------------
%% @doc
%%  Creates a new element process with a default timeout value.
%% @end
%%--------------------------------------------------------------------
-spec create(pid()) -> supervisor:startchild_ret(). 
create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

%%--------------------------------------------------------------------
%% @doc
%%  Causes the process associated with pid to send a message containing its 
%%  held value to the calling process. If pid is not alive the caller 
%%  should handle it with a timeout.  
%% @end
%%--------------------------------------------------------------------
-spec fetch(pid()) -> {ok, pid()}.
fetch(Pid) ->
    gen_server:cast(Pid, {fetch,self()}).

%%--------------------------------------------------------------------
%% @doc
%%  Kills the process Pid and removes the mapping from the nn_msgids
%%  storage.
%% @end
%%--------------------------------------------------------------------
-spec delete(pid()) -> ok. 
delete(Pid) ->
    gen_server:cast(Pid, delete).

%%=============================================================
%% GEN_SERVER CALLBACKS
%%=============================================================

init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok,
     #state{value = Value,
	    lease_time = LeaseTime,
	    start_time = StartTime},
     nn_util:time_left(StartTime, LeaseTime)}.

handle_call(_, _, State) ->
    #state{lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    {reply, ok, State, TimeLeft}.

handle_cast(delete, State) ->
    {stop, normal, State};

handle_cast({fetch, Pid},State) ->
    #state{value = Value,
	   lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    Pid ! {ok, Value},
    {noreply, State, TimeLeft}.
    
handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_, _) ->
    nn_msgids:delete(self()),
    ok.

code_change(_, State, _) ->
    {ok, State}.


