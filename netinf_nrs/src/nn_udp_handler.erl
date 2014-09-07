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
%%% @author Kiril Goguev
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2012 by Thomas 
%%%-------------------------------------------------------------------
-module(nn_udp_handler).

-behaviour(gen_server).

%% API
-export([start_link/0, send/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(MCIP, {225, 4, 5, 6}).


-define(PORT, 2345).

-define(SERVER, ?MODULE). 

-record(state, {
	 ip_list = undefined :: undefined | [{pid(), inet:ip_address()}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


send(EncodedMsg)->
    ?SERVER ! {ok, self(), EncodedMsg}.

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
    setup_socket(),
    {ok, #state{ip_list = []}}.

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
handle_info({udp, _, Ip, _, Msg}, State) ->
   {ok, OwnIp} = inet_parse:address(nn_util:get_ipaddr()),
    case Ip of 
	    OwnIp ->
		       {noreply, State};
		_->
		       {ok, Pid} = nn_message_handler:spawn(nn_udp_formatting),
		       nn_message_handler:handle_reply_udp(Pid, Msg),
		       IpList = State#state.ip_list,
		       {noreply, State#state{ip_list = [{Pid,Ip}|IpList]}}
	end;
handle_info({ok, Pid, Reply}, State) ->
    nn_logger_server:log(verbose, ?MODULE, handle_info_reply, Reply),
    IpList = State#state.ip_list,
    send_msg(Reply, proplists:get_value(Pid, IpList)),
    {noreply, State#state{ip_list = proplists:delete(Pid, IpList)}}.

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

setup_socket() ->
    {ok, UDP_Socket}=gen_udp:open(?PORT,
			[{reuseaddr, true},
			 {multicast_loop, false},
			 {multicast_if, {0,0,0,0}},
			 {multicast_ttl, 4}
			 ]),
    inet:setopts(UDP_Socket, [{add_membership, {?MCIP, {0, 0, 0, 0}}}]).

send_msg(error, ok) ->
    ok;
send_msg(Msg, Ip) ->
    Opts = [{reuseaddr, true}, {broadcast, true}, {active, true}],
    {ok, Socket} = gen_udp:open(0, Opts),
    nn_logger_server:log(verbose, ?MODULE, 
    send_msg, ["sending to ", Ip, " and the msg is ", Msg]),
    case Ip of 
	undefined ->
	    ok = gen_udp:send(Socket,
			      ?MCIP,
			      ?PORT, 
			      Msg);
	_->
	    ok 

    end,
    gen_udp:close(Socket).
