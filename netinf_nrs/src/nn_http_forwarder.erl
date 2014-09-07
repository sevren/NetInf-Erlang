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
%%% @author Alex Lindholm 
%%% @author Thomas Nordström
%%% @author Marcus Ihlar
%%% @doc
%%% This module forwards the Netinf Get and Search messages to other NRS's 
%%% on the network if the NDO is not present on the local NRS. 
%%% @end
%%% Created : 19 Nov 2012 by Alex <alex@Alexcomp>
%%%-------------------------------------------------------------------
-module(nn_http_forwarder).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, spawn/0, forward_request/6, stop/1]).

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
    {ok, Pid} = nn_sub_supervisor:start_child(http_forwarder),
    nn_logger_server:log(verbose, ?MODULE, spawn, ["Pid: ", Pid]),
    {ok, Pid}.

%%----------------------------------------------------------------------
%% @doc
%%  forwards the get or search request to the other known NRS's if the 
%%  desired NDO is not found in the local NRS.
%% @end
%%----------------------------------------------------------------------
-spec forward_request(pid(), atom(), nn_proto:proto(), 
		      term(), integer(), string()) -> {noreply, term()}.
forward_request(Pid, Req, Msg, MsgId, TTL, Dest) ->
    nn_logger_server:log(verbose, ?MODULE, forward_get,
			 "Cast to gen_server"),
    gen_server:cast(Pid, {Req, Msg, MsgId, TTL, Dest}).

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
handle_cast({get, Msg, MsgId, TTL, Dest}, State) ->
    send_get(MsgId, Msg, TTL, Dest),
    {noreply, State};
handle_cast({search, Msg, MsgId, TTL, Dest}, State) ->
    send_search(MsgId, Msg, TTL, Dest),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
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

notify_event_handler(Method, DecodedBody, MsgId) ->
    {ok, Pid} = nn_msgid_store:lookup(list_to_binary(MsgId)),
    nn_logger_server:log(verbose, ?MODULE, notify_event_handler,
			 ["Time to notify the event handler whats going on", Pid, MsgId]),
    case Pid of 
	no_match ->
	    no_match;
	_ ->
	    Msg = create_nn_message(Method, DecodedBody, MsgId),
	    nn_event_handler:handle_response(Pid, Msg) 
    end,
    stop(self()).

create_nn_message(get, DecodedBody, MsgId) ->
    nn_logger_server:log(verbose, ?MODULE, create_nn_message,
			 "get message"),
    Status = proplists:get_value(<<"status">>, DecodedBody),
    case Status of
	203 ->
	    Ni = proplists:get_value(<<"ni">>, DecodedBody),
	    Locations = proplists:get_value(<<"loc">>, DecodedBody),
	    Ext = proplists:get_value(<<"metadata">>, DecodedBody),
	    Proto = nn_proto:new(Ni, Locations, Ext, undefined),
	    Msg = nn_proto:new(msg, MsgId),
	    nn_proto:set([{proto, Proto}, {method, get}], Msg);
	200 ->
	    Ni = proplists:get_value(<<"ni">>, DecodedBody),
	    Locations = proplists:get_value(<<"loc">>, DecodedBody),
	    Ext = proplists:get_value(<<"metadata">>, DecodedBody),
	    Octets = proplists:get_value(<<"octets">>, DecodedBody),
	    Proto = nn_proto:new(Ni, Locations, Ext, undefined),
	    Msg = nn_proto:new(msg, MsgId),
	    nn_proto:set([{proto, Proto}, {method, get}, {octets, Octets}], Msg);
	_ ->
	    Msg = nn_proto:new(msg, MsgId),
	    nn_proto:set([{method, error}], Msg)
    end;
create_nn_message(search, DecodedBody, MsgId) ->
    nn_logger_server:log(verbose, ?MODULE, create_nn_message,
			 "search message"),
    Status = proplists:get_value(<<"status">>, DecodedBody),
    case Status of
	200 ->
	    Results = proplists:get_value(<<"results">>, DecodedBody),
	    Msg = nn_proto:new(msg, MsgId),
	    ProtoResults = [protofy(X) || X <- Results],
	    nn_proto:set([{proto, ProtoResults}, {method, search}], Msg);
	_ ->
	    Msg = nn_proto:new(msg, MsgId),
	    nn_proto:set([{method, error}], Msg)
    end;
create_nn_message(error, _, MsgId) ->
    Msg = nn_proto:new(msg, MsgId),
    nn_proto:set([{method, error}], Msg).
    

send_search(MsgId, Tokens, TTL, NRS)->
    nn_logger_server:log(verbose, ?MODULE, send_search,
			 ["Forwarding search: ", MsgId, Tokens, TTL, NRS]),
    case httpc:request(post, 
		       {"http://" ++ NRS ++ ":9999/netinfproto/search", 
			[], 
			"application/x-www-form-urlencoded",
			"msgid="++ MsgId ++ 
			    "&tokens=" ++ 
			    tl(lists:foldl(fun(X,Acc) -> 
						   " " ++ binary_to_list(X) ++ Acc end, "",Tokens)) ++
			    "&TTL=" ++ TTL ++
			    "&ext="
		       }, [], []) of
	{ok, {_, _, Body}} ->
	    DecodedBody = nn_util:decode(Body),
	    notify_event_handler(search, DecodedBody, MsgId);
	_ ->
	    nn_discovery_service:remove_ip(http, NRS),
	    notify_event_handler(error, unused, MsgId)
    end.
	    


send_get(MsgId, URI, TTL, NRS) ->
    nn_logger_server:log(verbose, ?MODULE, send_get,
			 ["Forwarding get: ", MsgId, URI, TTL, NRS]),
    case httpc:request(post, 
		       {"http://" ++ NRS ++ ":9999/netinfproto/get", 
			[], 
			"application/x-www-form-urlencoded",
			"URI="++ URI ++ "&msgid="++ MsgId ++ "&TTL=" ++ TTL ++ "&ext="
		       }, [], []) of
	{ok, {_, HList, ResponseBody}} ->
	    case proplists:get_value("content-type", HList) of
		"multipart/form-data; boundary="++Boundary ->
		    DecodedBody = nn_util:decode(Boundary, ResponseBody);
		_ ->
		    DecodedBody = nn_util:decode(ResponseBody)
	    end,
	    notify_event_handler(get, DecodedBody, MsgId);
	_ ->
	    nn_discovery_service:remove_ip(http, NRS),
	    notify_event_handler(error, unused, MsgId)
    end.

protofy({PropList}) ->
    EmptyProto = nn_proto:new(proplists:get_value(<<"ni">>, PropList)),
    % The ext will be formatted so that it has the stucture of a json:decode
    % return. That way it will be consistant with the way it is returned 
    % from internal databases. 
    nn_proto:set([
		  {ext, 
		   {[{<<"meta">>, proplists:get_value(<<"meta">>, PropList)}]}
		  }],
		 EmptyProto).
    
    
