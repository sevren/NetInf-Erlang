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
%%% @author Thomas Nordström
%%% @author Marcus Ihlar
%%% @doc
%%% This module defines the behaviour of the http server for a netinf
%%% application. It listens to incoming http requests and spawns
%%% message handlers which process the requests. It will also receive
%%% messages from the message handlers and return it to the corresponding
%%% client.  
%%% @end
%%% Created : 16 Oct 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(nn_http_handler).

-behaviour(cowboy_http_handler).

-include_lib("eunit/include/eunit.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

%%%====================================================================
%%% @doc 
%%% Initializes the http handler
%%% @end
%%%====================================================================
-spec init({atom(), atom()}, cowboy_req:req(), _) -> 
		  {ok, cowboy_req:req(), undefined_state}.
init({tcp, http}, Req, _) ->
    {ok, Req, undefined_state}.

%%%====================================================================
%%% @doc  
%%% handles an HTTP request by spawning a message handler and forwarding 
%%% HTTP requests to the message handler.
%%% @end
%%%====================================================================
handle(Req, State) ->
     nn_logger_server:log(verbose, ?MODULE, handle,
				 "Receiving http request"),
    {ok, Pid} = spawn_msghandler(),
    {ok, Req2} = forward_msghandler(Pid, Req),
    nn_logger_server:log(verbose, ?MODULE, handle,
				 "Got back from forward_msghandler"),
    {ok, ReplyReq} = send_response(Req2),
    {ok, ReplyReq, State}.

%%%====================================================================
%%% @doc  
%%% tears down the connection
%%% @end
%%%====================================================================
terminate(_, _) ->
    ok.

%%%=====================================================
%%% Internal functions
%%%====================================================

%%@doc spawns a msg handler
spawn_msghandler()->
    {ok, Pid}= nn_message_handler:spawn(nn_http_formatting),
    %nn_logger_server:log(verbose, ?MODULE, spawn_msgHandler,
%				 {ok, Pid, "Has been spawned"}),
    {ok, Pid}.
   

%%@doc forwards the message recieved from handle to the spawned msg handler
%% and waits for the reply
forward_msghandler(Pid, Req)->
    nn_logger_server:log(verbose, ?MODULE, forward_msgHandler,
			 "before handle request"),
    nn_message_handler:handle_request(Pid, Req),
    receive
	{ok, Pid, ReplyReq} ->
	    nn_logger_server:log(verbose, ?MODULE, forward_msgHandler,
				 "Has been forwarded"),
	    ReplyReq
    end.
	 
%%@doc send a response back to the real world
send_response({Status, Req})->
    nn_logger_server:log(verbose, ?MODULE, send_Response,
				 "Will be sent to requester"),
    cowboy_req:reply(Status, Req).


%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_, _, _, _) -> ok end),
    meck:new(nn_message_handler),
    meck:expect(nn_message_handler, spawn, fun() -> {ok, pid} end),
    meck:expect(nn_message_handler, handle_request,
		fun(_, _) -> {ok, {status, req}} end),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(_, _) -> {ok, req} end),
    meck:expect(cowboy_req, reply, fun(_, _, _, _) -> {ok, req} end).
    
clear_test_state() ->
    meck:unload(nn_logger_server),
    meck:unload(nn_message_handler),
    meck:unload(cowboy_req).

%%%=====================================================================
%%% Internal tests
%%%=====================================================================

setup() ->
    set_test_state().

teardown(_) ->
    clear_test_state().

internal_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun spawn_mh/1,
      fun send_resp/1
     ]}.

spawn_mh(_) ->
    S=spawn_msghandler(),
    [
     ?_assertMatch({ok, _}, S)
    ].

send_resp(_) ->
    S=send_response({a, b}),
    [
     ?_assertMatch({ok, _}, S)
    ].
