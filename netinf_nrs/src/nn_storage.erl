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
%%% @author Alexander Lindholm
%%% @author Kiril Goguev
%%% @doc
%%% This module defines the behaviour of the process that manages the
%%% NRS database. It provides functionality for adding, updating and
%%% retrieval of NetInf data e.g NDO locators.
%%% @end
%%% Created : 16 Oct 2012 by Marcus Ihlar <marcus@marcus>
%%% test for unpacker error
%%%-------------------------------------------------------------------
-module(nn_storage).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start/0,
	 stop/0,
	 publish/1, 
	 get/1, 
	 unpublish/1,
	 search/1,
	 flush/0,
	 set_db/1
	]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         terminate/2, 
	 code_change/3
	]).

-define(SERVER, ?MODULE).

-record(state, {current_database=undefined}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link() ->
     gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

set_db(NewDatabase)->
    gen_server:call(?SERVER, {set_db, NewDatabase}).

%%--------------------------------------------------------------------
%% @doc 
%% store/merge the NetInfObject in the storage. 
%% @end
%%--------------------------------------------------------------------
-spec publish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()}.
publish(NetInfObject) ->
    gen_server:call(?SERVER, {publish, NetInfObject}).

%%--------------------------------------------------------------------
%% @doc
%% returns the NetInfObject with the name Name if present
%% in the storage.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: string()) 
	 -> {ok, nn_proto:proto()} | {ok, no_match}.
get(Name) ->
    gen_server:call(?SERVER, {get, Name}).

%%--------------------------------------------------------------------
%% @doc 
%% removes the locators present in NetInfObject from the stored
%% NetInfObject, if it was the last occurance of locators the whole
%% object is removed.
%% @end
%%--------------------------------------------------------------------
-spec unpublish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()} | {ok, no_match}.
unpublish(NetInfObject) ->
    gen_server:call(?SERVER, {unpublish, NetInfObject}).

%%--------------------------------------------------------------------
%% @doc 
%% Searches the storage for items containing one or more of the 
%% tokens within their ext.
%% @end
%%--------------------------------------------------------------------
-spec search(Tokens :: list()) 
	 -> {ok, Matches :: list()} | {ok, no_match}.
search(Tokens) ->
    gen_server:call(?SERVER, {search, Tokens}).


%%--------------------------------------------------------------------
%% @doc 
%% Deletes all the entries in the database 
%% @end
%%--------------------------------------------------------------------
-spec flush() 
	 -> ok .
flush() ->
    gen_server:call(?SERVER, flush).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{current_database=undefined}}.
   
handle_call({set_db, NewDatabase}, _, State) ->
    NewDatabase:init(),
    {reply, ok, State#state{current_database = NewDatabase}};

handle_call({publish, NetInfObject}, _, State) ->
    Database = State#state.current_database,
    nn_logger_server:log(verbose, ?MODULE, handle_call_publish, [Database]),
    {ok, NewObject} = Database:publish(NetInfObject),
    case NewObject of
	NetInfObject ->
	    nn_stats:update(ndo_count, 1);
	_ ->
	    ok
    end,
    {reply, {ok, NewObject}, State};

handle_call({get, Name}, _, State) ->
    Database = State#state.current_database,
    {ok, Data} = Database:get(Name),
    {reply, {ok, Data}, State};

handle_call({unpublish, NetInfObject}, _, State) -> 
    Database = State#state.current_database,
    {ok, NewObject} = Database:unpublish(NetInfObject),
    {reply, {ok, NewObject}, State};

handle_call(flush, _, State) -> 
    Database = State#state.current_database,
    ok = Database:flush(),
    {reply, ok, State};

handle_call({search, Tokens}, _, State) -> 
    Database = State#state.current_database,
    Matches = Database:search(Tokens),
    {reply, Matches, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

