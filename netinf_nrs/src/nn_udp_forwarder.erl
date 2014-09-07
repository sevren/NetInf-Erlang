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
%%% @author Faroogh Hassan
%%% @doc
%%% This module forwards Get and search messages over UDP conergence layer.
%%% to 
%%% @end
%%% Created : 5 Dec 2012 by Kiril
%%%-------------------------------------------------------------------
-module(nn_udp_forwarder).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, spawn/0, forward_request/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%----------------------------------------------------------------------
%% @doc
%%  Spawns a new http_forwarder process.
%% @end
%%----------------------------------------------------------------------
-spec spawn() -> {ok, pid()}.
spawn() ->
    {ok, Pid} = nn_sub_supervisor:start_child(udp_forwarder),
    nn_logger_server:log(verbose, ?MODULE, spawn, ["Pid: ", Pid]),
    {ok, Pid}.

%%----------------------------------------------------------------------
%% @doc
%%  gen_server cast for a request.
%% @end
%%----------------------------------------------------------------------
forward_request(Pid, Method, Msg, MsgId) ->
    nn_logger_server:log(verbose, ?MODULE, forward_get,
			 "Cast to gen_server"),
    gen_server:cast(Pid, {Method, Msg, MsgId}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec stop(Pid) -> {stop, normal, State}
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    nn_logger_server:log(verbose, ?MODULE, handle_call,
				 "Im gonna die"),
    {stop, normal, State};
handle_cast({get, Msg, MsgId}, State) ->
    send_get(MsgId, Msg),
    {noreply, State};
handle_cast({search, Msg, MsgId}, State) ->
    send_search(MsgId, Msg),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_event_handler(EncodedMsg, MsgId) ->
    nn_udp_handler:send(EncodedMsg),
    nn_logger_server:log(verbose, ?MODULE, notify_event_handler,
			 ["Time to notify the event handler whats going on", MsgId]),
    stop(self()).


send_search(MsgId, Tokens)->
     nn_logger_server:log(verbose, ?MODULE, send_search,
			 ["Forwarding search: ", MsgId, Tokens]),
			
    Object = {[
	       {<<"version">>,<<"NetInfUDP/1.0">>},
	       {<<"msgType">>, <<"SEARCH">>},
	       {<<"msgId">>,list_to_binary(MsgId)},
	       {<<"tokens">>, [list_to_binary(Tokens)]}
	    ]},
    case json:encode(Object) of
    {ok, EncodedMsg} ->
	    notify_event_handler(EncodedMsg, MsgId);
	    _ ->
	    notify_event_handler(unused, MsgId)
    end.

	    


send_get(MsgId, URI) ->
    nn_logger_server:log(verbose, ?MODULE, send_get,
			 ["Forwarding get: ", MsgId, URI]),
    Object = {[
	     {<<"version">>,<<"NetInfUDP/1.0">>},
	     {<<"msgType">>, <<"GET">>},
	     {<<"uri">>, list_to_binary(URI)},
	     {<<"msgId">>,list_to_binary(MsgId)}
	    ]},
    case json:encode(Object) of
    {ok, EncodedMsg} ->
	    notify_event_handler(EncodedMsg, MsgId);
	    _ ->
	    notify_event_handler(unused, MsgId)
    end.
    

