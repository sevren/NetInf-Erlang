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
%%%  This module defines the behaviour of event handler processes. An
%%%  event handler receives a NetInf message and invokes the correct
%%%  internal service e.g updating the database.
%%% @end
%%% Created : 16 Oct 2012 by Marcus Ihlar <marcus@ihlar.se>
%%%-------------------------------------------------------------------
-module(nn_event_handler).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-define(STREAMID, <<"demo">>).

%% API Functions
-export([handle_request/2, 
	 start_link/0, 
	 spawn/0, 
	 kill/1, 
	 handle_response/2]).

%% gen_server callbacks 
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2,
	 code_change/3]).

%% Test helpers
-export([set_test_state/0, 
	 clear_test_state/0]).

-record(state, {
	  pid = undefined :: undefined | pid(),
	  pending_reqs = 0 :: integer(),
	  msgid = undefined :: term(), 
	  lease_time = 0 :: integer(),
	  start_time = 0 :: integer()
	 }).

%%%=====================================================================
%%% API
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc 
%%  Takes a NetInf request and performes the appropriate action. 
%%  The action performed will either update or fetch data from the database.
%% @end
%%----------------------------------------------------------------------
-spec handle_request(pid(), nn_proto:proto_msg()) -> 
			    ok.
handle_request(Pid, Msg) ->
    nn_stats:update(request_count, 1),
    nn_logger_server:log(verbose, ?MODULE, handle_request,
				 ["Handling request"]),
    gen_server:cast(Pid, {handle_request, self(), Msg}).

%%----------------------------------------------------------------------
%% @doc
%%  Spawns a new event_handler process.
%% @end
%%----------------------------------------------------------------------
-spec spawn() -> {ok, pid()}.
spawn() ->
    {ok, Pid} = nn_sub_supervisor:start_child(event_handler),
    nn_logger_server:log(verbose, ?MODULE, spawn, ["Pid: ", Pid]),
    nn_stats:update(active_count, 1),
    {ok, Pid}.

%%----------------------------------------------------------------------
%% @doc
%%  Kills the process given as argument.
%% @end
%%----------------------------------------------------------------------
-spec kill(pid()) -> ok.
kill(Pid) ->
    gen_server:cast(Pid, kill).

%%----------------------------------------------------------------------
%% @doc
%%  Should be called by supervisor
%% @end
%%----------------------------------------------------------------------
-spec start_link() -> {ok, undefined}.
start_link() ->
    gen_server:start_link(?MODULE, undefined, []).

%%--------------------------------------------------------------------
%% @doc
%%  Handles replies from forwarded msges
%% @end
%%--------------------------------------------------------------------
-spec handle_response(pid() | no_match, nn_proto:proto_msg()) -> ok.
handle_response(Pid, Msg) -> 
    gen_server:cast(Pid, {handle_response, Msg}).

%%%=====================================================================
%%% gen_server callbacks
%%%=====================================================================

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
init(_) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------e
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
handle_call(_, _, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------    
handle_cast(timeout, State) ->
    nn_stats:update(not_found_count, 1),
    ReplyMsg = nn_proto:set(
		 [
		  {method, error}, 
		  {proto, no_match}
		 ], nn_proto:new(msg, State#state.msgid)),
    nn_message_handler:handle_reply(State#state.pid, ReplyMsg),
    kill(self()),
    {noreply, State};
handle_cast({handle_request, Pid, Msg}, State) ->
    nn_logger_server:log(verbose, ?MODULE, handle_call,
			 "Genserver callback function"),
    case process_req(nn_proto:get(method, Msg), Msg) of
	{ok, forwarded, PendingReqs} ->
	    nn_stats:update(forwarded_reqs_count, 1),
	    nn_stats:update(forwarded_times_count, PendingReqs),
	    nn_logger_server:log(verbose, ?MODULE, handle_cast,
			 ["Forward"]),
	    MsgId =  nn_proto:get(msgid, Msg),
	    nn_msgid_store:insert(MsgId, self()),
	    Now = calendar:local_time(),
	    StartTime = calendar:datetime_to_gregorian_seconds(Now),
	    LeaseTime = 3,
	    {noreply, #state{pid = Pid,
	    			  pending_reqs = PendingReqs,
	    			  msgid = MsgId,
	    			  lease_time = LeaseTime,
	    			  start_time = StartTime},
	     nn_util:time_left(StartTime, LeaseTime)};
	{ok, no_match} ->
	    nn_stats:update(not_found_count, 1),
	    ReplyMsg = nn_proto:set(
			 [
			  {method, error}, 
			  {proto, no_match}
			 ], nn_proto:new(msg, nn_proto:get(msgid, Msg))),
	    return_and_die(Pid, ReplyMsg, State);
	{error, Error} -> 
	   
	    ReplyMsg = nn_proto:set(
			 [
			  {method, error}, 
			  {proto, Error}
			 ], nn_proto:new(msg, nn_proto:get(msgid, Msg))),
	    return_and_die(Pid, ReplyMsg, State);
	{ok, ReplyMsg} ->
	    
	    nn_stats:update(tot_hit_count, 1),
	    nn_stats:update(local_hit_count, 1),
	    return_and_die(Pid, ReplyMsg, State)
    end;

handle_cast(kill, State) ->
    nn_logger_server:log(verbose, ?MODULE, kill, 
			 ["killing pid: ", self()]),
    {stop, normal, State};
handle_cast({handle_response, Resp}, State) ->
    io:format("~n in event handler ~p handle_response ~n", [self()]),
    nn_logger_server:log(
      verbose, ?MODULE, handle_cast, 
      ["Got a handle response: ", Resp, State]),
    case nn_proto:get(method, Resp) of
	error ->
	    case State#state.pending_reqs of
		1 ->
		    nn_stats:update(not_found_count, 1),
		    Msg = nn_proto:new(msg, nn_proto:get(msgid, Resp)),
		    nn_message_handler:handle_reply(State#state.pid, 
						   nn_proto:set([{method, error},
								 {proto, no_match}], Msg)),
		    kill(self()),
		    {noreply, State};
		N ->
		    LeaseTime = State#state.lease_time,
		    StartTime = State#state.start_time,
		    {noreply, State#state{pending_reqs = N - 1},
		     nn_util:time_left(StartTime, LeaseTime)}
	    end;
	get ->
	    nn_logger_server:log(
	      verbose, ?MODULE, handle_cast, 
	      ["Get response"]),
	    nn_message_handler:handle_reply(State#state.pid, Resp),
	    process_req(publish, Resp),
	    kill(self()),
	    nn_stats:update(tot_hit_count, 1),
	    {noreply, State} ;
	search ->
	    nn_logger_server:log(
		      verbose, ?MODULE, handle_cast, 
		      ["Search response: ", nn_proto:get(msgid, Resp)]),
	    nn_message_handler:handle_reply(State#state.pid, Resp),
	    kill(self()),
	    nn_stats:update(tot_hit_count, 1),
	    {noreply, State};
	Undef ->
	    nn_logger_server:log(error, nn_event_handler, handle_cast, 
				 ["Cannot handle response of method: ",
				  Undef, Resp])
    end;
handle_cast({handle_content_put, Name, Data}, State) ->
    CH = nn_content_handler:spawn(),
    nn_content_handler:store_content_without_validating(CH, Name, Data),
    nn_content_handler:kill(CH),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {ok, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    nn_logger_server:log(
      verbose, ?MODULE, handle_info, ["Timed out: ", State]),
    Msg = nn_proto:new(msg, State#state.msgid),
    nn_message_handler:handle_reply(State#state.pid, 
				    nn_proto:set([{method, error},
						  {proto, no_match}], Msg)),
    {stop, normal, State};

handle_info({replyudp, Msg}, State) ->
    nn_logger_server:log(
      verbose, ?MODULE, handle_info, ["replyudp ", Msg]),
    {ok, State};

handle_info(_, State) -> 
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_, _) ->
    nn_stats:update(active_count, -1),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_, State, _) ->
    {ok, State}.

%%%=====================================================================
%%% Internal functions 
%%%=====================================================================

return_and_die(Pid, ReplyMsg, State) ->
    nn_message_handler:handle_reply(Pid, ReplyMsg),
    kill(self()),
    {noreply, State#state{pid = Pid}}.

process_req(get, Msg) ->
    case nn_msgid_store:lookup(nn_proto:get(msgid, Msg)) of
	{ok, no_match} ->
	    URI = nn_proto:get(proto, Msg),
	    case nn_hash_validation:parse_name(URI) of
		{?STREAMID, _} ->
		    chunked_data(Msg, URI);
		{_, _} ->	
		    case nn_storage:get(URI) of 
			{ok, no_match} ->
			    nn_message_forwarder:forward(Msg);
			{ok, Req} ->
			    nn_logger_server:log(verbose, ?MODULE, process_req, ["Get", Req]),
			    check_cache(Msg, Req, URI)
		    end
	    end;
	{ok, _} ->
	    {ok, no_match}
    end;
process_req(publish, Msg) ->
    NetInfMsg = nn_proto:get(proto, Msg),
    nn_logger_server:log(verbose, ?MODULE, process_req, 
			 ["Publish", NetInfMsg]),
    case nn_proto:get(octets, Msg) of
	undefined ->
	    Ts = nn_util:create_timestamp(),
	    {ok, Rp} = nn_storage:publish(nn_proto:set([{time_stamp, Ts}],
						       NetInfMsg)),
	    {ok, nn_proto:set([{proto, Rp}], Msg)};
	Binary ->
	    publish_and_store(Msg, NetInfMsg, Binary)
    end;
process_req(unpublish, Msg) ->
    Proto = nn_proto:get(proto, Msg),
    {ok, Rp} = nn_storage:unpublish(Proto),
    {ok, nn_proto:set([{proto, Rp}], Msg)};
process_req(search, Msg) ->
    case nn_msgid_store:lookup(nn_proto:get(msgid, Msg)) of
	{ok, no_match} ->
	    Tokens = nn_proto:get(tokens, Msg),
	    nn_logger_server:log(verbose, ?MODULE, process_req, ["Search", Tokens]),
	    case nn_storage:search(Tokens) of
		{ok, no_match} ->
		    nn_message_forwarder:forward(Msg);
		{ok, Protos} ->
		    {ok, nn_proto:set([{proto, Protos}], Msg)}
	    end;
	{ok, _} ->
	    {ok, no_match}
    end;
process_req(reply, Msg) ->
    Id = nn_proto:get(msg_id, Msg),
    {ok, Pid} = nn_msgid_store:lookup(Id),
    handle_response(Pid, Msg).
   
publish_and_store(Msg, NetInfMsg, Binary) ->
    Pid = nn_content_handler:spawn(),
    case nn_content_handler:validate_and_store_content(
	   Pid, {nn_proto:get(name, NetInfMsg), Binary}) of
	ok ->
	    Ts = nn_util:create_timestamp(),
	    {ok, Pr} = nn_storage:publish(nn_proto:set([{time_stamp, Ts}],
						       NetInfMsg)),
	    Resp = {ok, nn_proto:set([{proto, Pr}], Msg)};
	{error, invalid_hash} ->
	    Resp = {error, invalid_hash}
    end,
    nn_content_handler:kill(Pid),
    Resp.

check_cache(Msg, Req, URI) ->
    Pid = nn_content_handler:spawn(),
    case nn_content_handler:fetch_content(Pid, URI) of
	{ok, file_not_found} ->
	    Resp = {ok, nn_proto:set([{proto, Req}], Msg)} ;
	{ok, Data} ->
	    Resp = {ok, nn_proto:set([{proto, Req}, {octets, Data}], Msg)}
    end,
    nn_content_handler:kill(Pid),
    Resp.

chunked_data(Msg, URI) ->
    Pid = nn_content_handler:spawn(),
    case nn_content_handler:fetch_content(Pid, URI) of
	{ok, file_not_found} ->
	    Resp = {ok, no_match};
	{ok, Data} ->
	    EmptyReq = nn_proto:set([{time_stamp, nn_util:create_timestamp()}], 
				    nn_proto:new(URI)), 
	    Resp = {ok, nn_proto:set([{proto, EmptyReq}, {octets, Data}], Msg)}
    end,
    nn_content_handler:kill(Pid),
    Resp.
    
%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_, _, _, _) -> ok end),
    meck:new(nn_storage),
    meck:expect(nn_storage, get, 
		fun(Name) -> {ok, nn_proto:new(Name)} end),
    meck:expect(nn_storage, publish, fun(Msg) -> {ok, Msg} end),
    meck:expect(nn_storage, unpublish, fun(Msg) -> {ok, Msg} end),
    meck:new(nn_sub_supervisor),
    meck:expect(nn_sub_supervisor, start_child, 
		fun(_) -> nn_event_handler:start_link() end),
    meck:new(nn_content_handler),
    meck:expect(nn_content_handler, spawn, fun() -> pid end),
    meck:expect(nn_content_handler, fetch_content, 
		fun (_, "Real") -> {ok, <<"data">>} ;
		    (_, "Fake") -> {ok, file_not_found} ;
		    (_, _) -> {ok, file_not_found}
		end),
    meck:expect(nn_content_handler, store_content, 
		fun(_, {"Real", _}) -> ok ;
		   (_, {"Fake", _}) -> {error, invalid_hash} ;
		   (_, _) -> ok
					   
		end),
    meck:expect(nn_content_handler, kill, fun(_) -> ok end).
			
clear_test_state() ->
    meck:unload(nn_logger_server),
    meck:unload(nn_storage),
    meck:unload(nn_content_handler),
    meck:unload(nn_sub_supervisor).


%%%=====================================================================
%%% Internal tests
%%%=====================================================================

%% setup() ->
%%     set_test_state(),
%%     nn_proto:new("ni:///sha-256;Name").

%% teardown(_) ->
%%     clear_test_state().

%% internal_test_() ->
%%     {foreach,
%%      fun setup/0,
%%      fun teardown/1,
%%      [
%%       fun procreq/1,
%%       fun cache/1,
%%       fun pubstore/1
%%      ]}.

%% procreq(Msg) ->
%%     Gr = process_req(get, "ni:///sha-256;Name"),
%%     Pr = process_req(publish, Msg),
%%     Upr = process_req(unpublish, Msg),
%%     [
%%      ?_assertMatch({ok, _}, Gr),
%%      ?_assertMatch({ok, _}, Pr),
%%      ?_assertMatch({ok, _}, Upr),
%%      ?_assertNotEqual(Msg, Pr)
%%     ].

%% cache(_) ->
%%     Real = "Real",
%%     RealMsg = nn_proto:new(Real),
%%     Fake = "Fake",
%%     FakeMsg = nn_proto:new(Fake),
%%     GoodRes = check_cache(RealMsg, Real),
%%     BadRes = check_cache(FakeMsg, Fake),
%%     [
%%      ?_assertMatch(GoodRes, {ok, RealMsg, <<"data">>}),
%%      ?_assertMatch(BadRes, {ok, FakeMsg})
%%     ].

%% pubstore(_) ->
%%     Real = "Real",
%%     RealMsg = nn_proto:new(Real),
%%     Fake = "Fake",
%%     FakeMsg = nn_proto:new(Fake),
%%     GoodRes = publish_and_store(RealMsg, <<"data">>),
%%     BadRes = publish_and_store(FakeMsg, <<"data">>),
%%     [
%%      ?_assertMatch({ok, _}, GoodRes),
%%      ?_assertMatch({error, invalid_hash}, BadRes)
%%     ].

    
