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
%%%  Storage module that stores keys and processes holding the values
%%%  associated with the keys. 
%%% @end
%%% Created : 19 Nov 2012 by Marcus Ihlar

-module(nn_msgids).

-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1
	 ]).

-define(TABLE_ID, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%%  Initializes the storage. Must be called before any other 
%%  use of this module.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ets:tid() | atom().
init() ->
    nn_logger_server:log(
      verbose, nn_msgids, init, "Initializing ets storage"),
    ets:new(?TABLE_ID, [public, named_table]).

%%--------------------------------------------------------------------
%% @doc
%%  Inserts an element pid and a key.
%% @end
%%--------------------------------------------------------------------
-spec insert(term(), pid()) -> ok.
insert(Key, Pid) ->
    nn_stats:add_to_list(msgid_list, Key),
    ets:insert(?TABLE_ID, {Key, Pid}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Returns the pid of the element process associated with the key or
%%  no_match if not present.
%% @end
%%--------------------------------------------------------------------
-spec lookup(term()) -> {ok, pid()} | {ok, no_match}.
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
	[{Key, Pid}] ->
		{ok, Pid};
	     [] ->
		{ok, no_match}
	end.

%%--------------------------------------------------------------------
%% @doc
%%  Deletes the key pid mapping if present.
%% @end
%%--------------------------------------------------------------------
-spec delete(pid()) -> ok.
delete(Pid) ->
    MsgIdList = ets:match(?TABLE_ID, {'$1', Pid}),
    case MsgIdList of
	[[MsgId]] -> 
	    nn_stats:remove_from_list(msgid_list, MsgId);
	_ ->
	    ok
    end,
    ets:match_delete(?TABLE_ID, {'_', Pid}),
    ok.

