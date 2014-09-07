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
%%%  This module provides the discovery service for other netinf nrs servers
%%%  regardless of thier implementation language using 
%%%  Simple service Discovery protocol. 
%%% @end
%%% Created : 15 Nov 2012 by Kiril 
%%%-------------------------------------------------------------------
-module(nn_discovery_service).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, 
	 start/1, 
	 add_ip/1,
	 add_ip/2,
	 remove_ip/2,
	 get_ips/0, 
	 get_ipaddr/0, 
	 parse_ip/1,
	 set_cl/1,
	 get_cl/0,
	 kill/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% define the ssdp multicast ip based on
%% http://www.iana.org/assignments/
%% multicast-addresses/multicast-addresses.xml
-define(MCIP, {239, 255, 255, 250}).

%% define the ssdp port
-define(MCPORT, 1900).
-define(SERVER, ?MODULE). 


-record(state, {netinf_nodes=undefined :: undefined | list(), 
		convergence_layers=undefined :: undefined | list()
	       }).

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


%%----------------------------------------------------------------------
%% @doc 
%%  Starts a gen_udp server listening by default on ip 
%%  239.255.255.250 and port 1900
%% Start function code was taken from an example here: 
%% http://stackoverflow.com/questions/78826/erlang-multicast
%% @end
%%----------------------------------------------------------------------
-spec start(atom()) ->{ Socket :: string(), Pid :: pid()}
			  | {error, Reason :: string()}.
start(Module)->
    UDP_Socket = setup_socket(),
    Pid = spawn(Module, recieved_udpmsg, []),
    gen_udp:controlling_process(UDP_Socket, Pid),
    {UDP_Socket, Pid}.

%%----------------------------------------------------------------------
%% @doc 
%% adds an Ip to the list of Ip addresses
%% @end
%%----------------------------------------------------------------------
-spec add_ip(list(), string()) -> ok| {reply, ok, list()}.
add_ip(CL, Ip)->
    case application:get_env(netinf_nrs, discovery) of
	{ok, off} ->
	    ok;
	_ ->
	    gen_server:call(?SERVER, {add_ip, CL, Ip})
    end.

%%----------------------------------------------------------------------
%% @doc 
%% adds an Ip to the list of Ip addresses
%% @end
%%----------------------------------------------------------------------
-spec add_ip(string()) -> ok| {reply, ok, list()}.
add_ip(Ip)->
    case application:get_env(netinf_nrs, discovery) of
	{ok, off} ->
	    ok;
	_ ->
	    gen_server:call(?SERVER, {add_ip, Ip})
    end.

%%----------------------------------------------------------------------
%% @doc 
%% removes an Ip from the list of Ip addresses
%% @end
%%----------------------------------------------------------------------
-spec remove_ip(list(), string()) -> {noreply, undefined|list()}.
remove_ip(CL, Ip) ->
    gen_server:cast(?SERVER, {remove_ip, CL, Ip}).

%%----------------------------------------------------------------------
%% @doc 
%% returns the list of ip addresses on the ntwork who are running the NRS
%% @end
%%----------------------------------------------------------------------
-spec get_ips() -> {reply, {ok, undefined|list()}, list()}.
get_ips()->	  
    gen_server:call(?SERVER, {get_ips, []}).

%%----------------------------------------------------------------------
%% @doc
%%  Kills the process given as argument.
%% @end
%%----------------------------------------------------------------------
-spec kill(pid()) -> ok.
kill(Pid) ->
    gen_server:cast(Pid, kill).

%%--------------------------------------------------------------------
%% @doc
%%  Returns the parsed ipv4 or ipv6 from the system in erlang format 
%% @end
%%--------------------------------------------------------------------

-spec get_ipaddr() -> string().   
get_ipaddr()->
 {ok, Ipaddrs} = inet:getif(),
    {Ip, _, _} = hd(Ipaddrs),    
    inet_parse:ntoa(Ip).

%%--------------------------------------------------------------------
%% @doc
%%  Parses the ip address, convergence layer and the Header from the 
%%  recieved udp packet
%% @end
%%--------------------------------------------------------------------

-spec parse_ip(Packet :: iolist()) -> 
		      {CL :: list(), Header :: string(), Ip :: string()}.
parse_ip(Packet)->
    case re:run(Packet,
		"(CL:)(.*),(Nrs):(([0-9A-Fa-f]*[\.:]*)*)",[{capture,[2, 3, 4], list}]) of
	
    {match,[CL, Header, Ip]}->
	    {CL, Header, Ip};
	nomatch ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @doc
%% adds a converegence layer to the list of convergenc layers
%% @end
%%--------------------------------------------------------------------
-spec set_cl(list()) -> {reply, ok, undefined|list()}.
set_cl(CL) ->
    gen_server:call(?SERVER, {set_cl, CL}).

get_cl()->    
    gen_server:call(?SERVER, {get_cl,[]}).

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
%%-------------------------------------------------------------------
init([]) ->
    start(nn_discovery_client),
    {ok, #state{netinf_nodes = undefined, convergence_layers = undefined}}.

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
handle_call({set_cl, CL}, _, State) ->
    {reply, ok, State#state{convergence_layers = CL}};

handle_call({add_ip, SupportedCL, Ip}, _, State) ->
    case State#state.netinf_nodes of
	undefined ->
	    {ok, OwnIp} = inet_parse:address(lists:flatten(get_ipaddr())),
	    case  Ip =/= OwnIp  of
		true ->
		    {reply, ok, State#state{netinf_nodes=
						[erlang:append_element({SupportedCL}, [Ip])|[]]}};
		false ->
		    {reply, ok, State}
	    end;
	_ ->
	    {ok, OwnIp} = inet_parse:address(lists:flatten(get_ipaddr())),
	    case Ip =/= OwnIp of 
		true ->
		    case lists:member(SupportedCL, proplists:get_keys(
						     State#state.netinf_nodes)) of 
			true ->
			    ListofKeys=proplists:get_keys(State#state.netinf_nodes),
			    NewState=check_ip_duplication (Ip,State, ListofKeys, SupportedCL),
			    {reply, ok, NewState};
			false ->
			    {reply, ok, State#state{netinf_nodes=
							[erlang:append_element
							   ({SupportedCL},[Ip])|State#state.netinf_nodes]}}
				
		    end;
		false ->
		     {reply, ok, State}
	    end
			
    end;    

handle_call({get_ips, []}, _, State)->
    case application:get_env(static_peers) of
	undefined ->
	    Reply = State#state.netinf_nodes;
	{ok, StaticPeers} ->
	    Reply = merge_address_lists(StaticPeers, State#state.netinf_nodes)
    end,
    {reply, {ok, Reply}, State};

handle_call({get_cl, []}, _, State)->
    {reply,{ok, State#state.convergence_layers}, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({remove_ip, CL, Ip}, State) ->
    case State#state.netinf_nodes of
	undefined ->
	    {noreply, State};
	NodePropList ->
	    case proplists:get_value(CL, NodePropList) of
		undefined ->
		    {noreply, State};
		IpList ->
		    CleanPropList = proplists:delete(CL, NodePropList),
		    CleanIpList = lists:filter(fun(X) ->
						       inet_parse:ntoa(X) =/= Ip end, IpList),
		    generate_remove_ip_response(CleanPropList, CleanIpList, CL, State)
	    end
    end;

handle_cast(kill, State) ->
    {stop, normal, State}.

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

%%%=====================================================================
%%% Internal functions 
%%%=====================================================================
setup_socket()->
    {ok, UDP_Socket}=gen_udp:open(?MCPORT,
			[{reuseaddr, true},
			 {multicast_loop, false},
			 {multicast_if, {0,0,0,0}},
			 {multicast_ttl, 4}
			 ]),
    inet:setopts(UDP_Socket, [{add_membership, {?MCIP, {0, 0, 0, 0}}}]),
    UDP_Socket.

check_ip_duplication(_, State, [], _) -> 
    State;
check_ip_duplication(Ip, State, [Hd|Tl], SupportedCL) -> 
    case SupportedCL =:= Hd of
	true->
	    case lists:member(Ip, proplists:get_value(Hd,
					      State#state.netinf_nodes)) of  
		false ->
		    NewState=adding_ip(Ip, State, Hd),
		    check_ip_duplication(Ip, 
					 State#state{netinf_nodes=NewState}, 
					 Tl, 
					 SupportedCL)	;
		true ->
		     check_ip_duplication(Ip, State, Tl, SupportedCL)
	    end;
	false ->
	    check_ip_duplication(Ip, State, Tl, SupportedCL)
    end.

adding_ip(Ip, State, Clkey) ->	
    OldIpList=proplists:get_value(Clkey, State#state.netinf_nodes),
    UpdatedIpList=[Ip|OldIpList],
    ReplacementTuple=erlang:append_element({Clkey}, UpdatedIpList),
    FixedList=lists:keyreplace(
		Clkey,
		1,
		State#state.netinf_nodes,
		ReplacementTuple),
    FixedList.
	    
merge_address_lists(undefined, List)->
    List;
merge_address_lists(List, undefined) ->
    List;
merge_address_lists([], List)->
    List;
merge_address_lists([{Key, Value}|Tail], List) ->
    SortedValue = lists:usort(proplists:get_value(Key, List, [])),
    SortedValue2 = lists:usort(Value),
    MergedValues = lists:umerge(SortedValue, SortedValue2),
    CleanList = proplists:delete(Key, List),
    merge_address_lists(Tail, [{Key, MergedValues}|CleanList]).
    
    
    



generate_remove_ip_response([], [], _, State) ->
    {noreply, State#state{netinf_nodes = undefined}};
generate_remove_ip_response(CleanPropList, [], _, State)->
    {noreply, State#state{netinf_nodes = CleanPropList}};
generate_remove_ip_response(CleanPropList, CleanIpList, CL, State) ->
    {noreply, State#state{netinf_nodes = [{CL, CleanIpList}|CleanPropList]}}.
      

%%%=====================================================================
%%% Internal tests
%%%=====================================================================

setup() ->
    {_, Pid}=start_link(),
    Convergence_layers=["http","udp"],
    nn_discovery_service:set_cl(Convergence_layers),
    add_ip(http, {130, 238, 15, 214}),
    Pid.

teardown(_) ->
    ok.

internal_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun check_ip_dup/1,
      fun add_ip_to_server/1
     ]}.



check_ip_dup(_)->
    Ip={130, 238, 15, 214},
    State =  {state, [{http, [{130, 238, 15, 214}]}], [http, udp]},
    ResultState=check_ip_duplication(Ip, State, [http, udp], http),
    Ip2 = {130, 238, 15, 226},
    ExpectedState =  {state,
		      [{http, [{130, 238, 15, 226}, {130, 238, 15, 214}]}], [http, udp]},
    ResultState2 =check_ip_duplication(Ip2, State, [http, udp], http),
	[ 
	  ?_assertEqual(State, ResultState),
	  ?_assertEqual(ExpectedState,ResultState2)
	].

add_ip_to_server(_)->
    Ip={130, 238, 15, 226},
    State = {state, [{http, [{130, 238, 15, 214}]}], [http, udp]},
    Result = adding_ip(Ip, State, http),
    ExpectedResult =  [{http, [{130, 238, 15, 226}, {130, 238, 15, 214}]}],
    [
     ?_assertEqual(ExpectedResult,Result)
    ].
