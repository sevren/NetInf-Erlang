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
%%%  This module serves as an interface to a timed storage.
%%%  Elements will be removed after a specified or 
%%%  default ammount of time has passed.
%%% @end
%%% Created : 19 Nov 2012 by Marcus Ihlar <marcus@marcus>

-module(nn_msgid_store).

-export([insert/2, insert/3, lookup/1, delete/1]).

%%--------------------------------------------------------------------
%% @doc
%%  Stores a key value pair with a default timeout value. Will return
%%  an error if the key is already present.
%% @end
%%--------------------------------------------------------------------
-spec insert(term(), term()) -> ok | {error, _Reason}.
insert(Key, Value) ->
    create_or_err(Key, fun() -> 
			       {ok, Pid} = nn_msgide:create(Value),
			       nn_msgids:insert(Key, Pid) 
		       end).

%%--------------------------------------------------------------------
%% @doc
%%  Stores a key value pair with a specified timeout value.
%%  Will return an error if the key is already present.
%% @end
%%--------------------------------------------------------------------
-spec insert(term(), term(), integer()) -> ok | {error, _Reason}.
insert(Key, Value, Time) ->
    create_or_err(Key, fun() -> 
			       {ok, Pid} = nn_msgide:create(Value, Time),
			       nn_msgids:insert(Key, Pid) 
		       end).

%%--------------------------------------------------------------------
%% @doc
%%  Returns the value of the specified key if present.
%% @end
%%--------------------------------------------------------------------
-spec lookup(term()) -> {ok, term()} | {ok, no_match}.
lookup(Key) ->
    case nn_msgids:lookup(Key) of
	{ok, no_match} ->
	    {ok, no_match} ;
	{ok, Pid} ->
	    nn_msgide:fetch(Pid), 
	    % avoid potential race conditions. see nn_msgide.erl 
	    receive
		Value ->
		    Value
	    after 5 ->
		    {ok, no_match}		
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Removes the pair with the provided key if present.
%% @end
%%--------------------------------------------------------------------
-spec delete(term()) -> ok.
delete(Key) ->
    case nn_msgids:lookup(Key) of
	{ok, no_match} ->
	    ok ;
	{ok, Pid} ->
	    nn_msgide:delete(Pid)
    end.

%%%===========================================================
%%% Internal functions
%%%===========================================================

create_or_err(Key, InsertFun) ->
    case nn_msgids:lookup(Key) of 
	{ok, no_match} ->
	    InsertFun();
	{ok, _} ->
	    {error, id_present}
    end.
