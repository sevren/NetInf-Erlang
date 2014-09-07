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
%%% @author Thomas Nordström
%%% @author Faroogh Hassan
%%% @author Marcus Ihlar
%%% @author Kiril Goguev
%%% @doc
%%% This modules defines the behaviour for message handling processes.
%%% A message handler is responsible for receiving and converting network
%%% layer messages, i.e http or UDP. Converted messages are forwarded to
%%% event handlers or back to network layer handlers. A network layer 
%%% message is converted using functionality defined in the 
%%% nn_message_formatter module.
%%% @end
%%% Created : 16 Oct 2012 by Thomas Nordström and Faroogh Hassan
%%%-------------------------------------------------------------------
-module(nn_message_handler).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-export([spawn/1, 
	 handle_request/2, 
	 handle_reply_udp/2, 
	 handle_reply/2, 
	 kill/1, 
	 start_link/1]).

%% Test helpers
-export([set_test_state/0, clear_test_state/0]).


-record(state, 
	{
	  formatter = undefined :: atom(),
	  cl_handler_pid = undefined :: undefined | pid(),
	  cl_data = undefined :: undefined | cowboy_req:req()
	}).

%%--------------------------------------------------------------------
%% @doc
%% starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} |
		      ignore |
		      {error, {already_started, pid()} |
		       term()}.
start_link(Formatter) ->
    gen_server:start_link(?MODULE, [Formatter], []).


%%--------------------------------------------------------------------
%% @doc
%% This function will have a supervisor process start a new 
%% message handler process. 
%% @end
%%--------------------------------------------------------------------

-spec spawn(atom()) -> {ok, pid()}.
spawn(Formatter) ->
    log(verbose, spawn, 'start a new message handler process'),
    nn_sub_supervisor:start_child({message_handler, Formatter}).


%%--------------------------------------------------------------------
%% @doc
%% This function will process an incoming network layer request
%% @end
%%--------------------------------------------------------------------

-spec handle_request(pid(), cowboy_req:req()) -> {noreply, term()}.
handle_request(Pid, Req)->
    gen_server:cast(Pid, {request, Req, self()}).

%%--------------------------------------------------------------------
%% @doc
%% This function will process an incoming network udp layer request
%% @end
%%--------------------------------------------------------------------

-spec handle_reply_udp(pid(), term()) -> {noreply, term()}.
handle_reply_udp(Pid, UdpReply)->
    gen_server:cast(Pid, {replyudp, UdpReply, self()}).



%%--------------------------------------------------------------------
%% @doc
%% This function will process an incoming internal reply
%% @end
%%--------------------------------------------------------------------

-spec handle_reply(pid(), cowboy_req:req()) -> {ok, cowboy_req:req()}.
handle_reply(Pid, Req)->
    gen_server:cast(Pid, {reply, Req}).

% initialize the message handler
init([Formatter])->
    {ok, #state{formatter = Formatter}}.

log(Level, Action, Msg) ->
    nn_logger_server:log(Level, ?MODULE, Action, Msg).

handle_call(_, _, State) ->
    {noreply, State}.
 
handle_cast({request, Req, Pid}, State) -> 
    log(verbose, 
	 after_create_message, ["about to send reply back to ", 
				State#state.formatter]),
    case do_request(Req, State#state.formatter) of
	{error, Reply} ->
	    Pid ! {ok, self(), {ok, Reply}},
	    kill(self()) ;
	_ -> ok
    end,
    {noreply, State#state{cl_handler_pid = Pid, cl_data = Req}};
handle_cast({reply, Msg}, State) ->
    Formatter = State#state.formatter,
    log(verbose, 
	calling_Formatter_create_message, ["processing  ", Formatter]),
    Reply = Formatter:create_message(Msg, State#state.cl_data), 
    Pid = State#state.cl_handler_pid,
    log(verbose, 
	handle_cast, ["done proccessing message, sending it to proccess ", Pid]),
    Pid ! {ok, self(), Reply},
    kill(self()),
    {noreply, State};


handle_cast({replyudp, UdpReply, Udp_Pid}, State) ->
    Formatter = State#state.formatter,
     log(verbose, 
	 calling_Formatter_create_message, ["processing  ", Formatter]),
    {ok, Msg} = Formatter:parse(UdpReply),

    case nn_proto:get(method, Msg) of
    	get ->
    	    forward_request(Msg),
	 {noreply, State#state{cl_handler_pid = Udp_Pid, cl_data = UdpReply}};
    	search ->
    	    forward_request(Msg),
	    {noreply, State#state{cl_handler_pid = Udp_Pid, cl_data = UdpReply}};
    	publish ->
	    forward_request(Msg),
	    {noreply, State#state{cl_handler_pid = Udp_Pid, cl_data = UdpReply}};
    	_ ->
	    {ok, Pid} = nn_msgid_store:lookup(nn_proto:get(msgid, Msg)),
	    case nn_proto:get(method, Msg) of 
		get_resp ->
		    FixedMsg=nn_proto:set([{method, get}], Msg),
		    nn_event_handler:handle_response(Pid, FixedMsg),
		    kill(self()),
		    {noreply, State};
		search_resp ->
		    FixedMsg=nn_proto:set([{method, search}], Msg),
		    nn_event_handler:handle_response(Pid, FixedMsg),
		    kill(self()),
		    {noreply, State};
		publish_resp ->
		    FixedMsg=nn_proto:set([{method, publish}], Msg),
		    nn_event_handler:handle_response(Pid, FixedMsg),
		    kill(self()),
		    {noreply, State}
	    end
    end;
    
handle_cast(stop, State) ->
    {stop, normal, State}.

do_request(Req, Formatter)->
    log(verbose, do_request, ["processing", Formatter, "Request"]),
    case Formatter:parse(Req) of
	{ok, Msg} ->
	    forward_request(Msg);
	{error, Reply} -> 
	    log(verbose, do_request,
		["could not parse", Formatter, " request into NetInf Request"]),
	    {error, Reply}
    end.

 % spawn an event handler
forward_request(Msg)->
    log(verbose, forward_request, 
	["spawn an Event Handler and forward a NetInf request to it"]),
    {ok, Eventpid} = nn_event_handler:spawn(),
    log(verbose, forward_request, 
	["spawned  Event Handler has pid: ", Eventpid]),
    nn_event_handler:handle_request(Eventpid, Msg).
 
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% This function will kill the specified process.
%% @end
%%--------------------------------------------------------------------

-spec kill(pid()) -> ok.
kill(Pid) ->
    log(verbose, kill, 'kill a message handler process'),
    gen_server:cast(Pid, stop).

%%%====================================
%%% Testing API
%%%====================================

%% sets up all mocks needed for running test in isolation.

set_test_state() ->
    meck:new(nn_message_formatting),
    meck:expect(nn_message_formatting, parse, 
		fun(_)-> {ok, get, "Name", "MessageID"} end),
    meck:expect(nn_message_formatting, parse, 
		fun(HTTPReq)-> {ok, publish, HTTPReq, "MessageID", "netinfproto"} end),
    meck:expect(nn_message_formatting, create_message,
		fun(_, HTTPReq) ->{ok, HTTPReq} end),
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_, _, _, _) -> ok end),
    meck:new(nn_sub_supervisor),
    meck:expect(nn_sub_supervisor, start_child, 
	fun(_) -> nn_message_handler:start_link() end),
    meck:new(nn_event_handler),
    meck:expect(nn_event_handler, spawn, fun() -> {ok, pid} end),
    meck:expect(nn_event_handler, handle_request, 
	     fun (_, Msg) -> {ok, Msg} end),
    meck:expect(nn_event_handler, kill, fun(_) -> ok end).
    
		      
clear_test_state() ->
   
    meck:unload(nn_message_formatting),
    meck:unload(nn_logger_server),
    meck:unload(nn_sub_supervisor),
    meck:unload(nn_event_handler).


%%%=====================================================================
%%% Internal tests
%%%=====================================================================

%% setup() ->
%% 	set_test_state().


%% teardown(_) ->
%%     clear_test_state().

%% internal_test_() ->
%%     {foreach,
%%      fun setup/0,
%%      fun teardown/1,
%%      [
%%       fun forwardreq/1
%%      ]}.

%% forwardreq(_) ->
%%     NetInfMessage=nn_proto:new("Name",["URI"],["EXT"],"TimeStamp"),
%%     {ok, GetResp} = forward_request(get, NetInfMessage),
%%     {ok, PubResp} = forward_request(publish, NetInfMessage),
    
%%     [
%%      ?_assertEqual(GetResp, NetInfMessage),
%%      ?_assertEqual(PubResp, NetInfMessage)
%%     ].

			
    
