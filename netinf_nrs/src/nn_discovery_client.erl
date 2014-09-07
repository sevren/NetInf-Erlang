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
%%%   This module provides the client functions for the discovery protocol
%%% @end
%%% Created : 15 Nov 2012 by Kiril <kiril@Sevren>
%%%-------------------------------------------------------------------
-module(nn_discovery_client).
-export([start/0,
	 recieved_udpmsg/0, 
	 send_ip/0, 
	 get_ip_list/0
	]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

-spec start() ->{ Socket :: string(), Pid :: pid()}
			  | {error, Reason :: string()}.
start()->
    nn_discovery_service:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Listener for udpmsgs that come into the server 
%% Adds an ip for each convergence layer
%% @end
%%--------------------------------------------------------------------

-spec recieved_udpmsg() -> ok | list().
recieved_udpmsg()->
     receive
         {udp, _, _, _, Packet} ->
	      case nn_discovery_service:parse_ip(Packet) of
		{CL, Header, Ip}->
		    case Header of
			"Nrs" ->
			    {ok, ErlangIp}=inet_parse:address(lists:flatten(Ip)),
			    ClAtomList=lists:map(fun(X)-> {list_to_atom(X), ErlangIp} end,
					string:tokens(CL, ",")),
			    lists:map(fun(X)-> 
					      {SupportedCL, Eip}=X,
					      nn_discovery_service:add_ip(SupportedCL, Eip)
				      end, ClAtomList)
		    end;
		    _ ->
		   ok
	    end,
             recieved_udpmsg();
         stop -> true;
         _-> 
            recieved_udpmsg()
	     
     end.


%%--------------------------------------------------------------------
%% @doc
%% Sends an ip to a multicast address 
%% the data msg is in the following format "CL:convergence_layers,Nrs:Ipaddr"
%% where the convergence_layers are the supported convergence layers 
%% configured in netinf_nrs_app and the Ipaddr is your
%%  ipv4 or ipv6 ip address.
%% @end
%%--------------------------------------------------------------------
-spec send_ip() -> ok | {error, Reason :: string()}.
send_ip()->
    Opts=[{reuseaddr,true},{broadcast, true},{active,true}],
    IpAddr= "Nrs:"++nn_discovery_service:get_ipaddr(),
    Data= ["CL:"++get_cl_list() | IpAddr],
    {ok,Socket} = gen_udp:open(0,Opts),
    ok = gen_udp:send(Socket,
		      {255,255,255,255},
		      1900, 
		      Data),
    gen_udp:close(Socket).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of ip's collected from the server in response 
%% to the broadcasted replies.
%% @end
%%--------------------------------------------------------------------
-spec get_ip_list() -> list().
get_ip_list()->
    {ok, IpList} = nn_discovery_service:get_ips(),
    IpList.

%%%=====================================================================
%%% Internal functions 
%%%=====================================================================
get_cl_list()->
    {ok, ClList}= nn_discovery_service:get_cl(),
    lists:map(fun(X)-> X++"," end,ClList).
  
%%%=====================================================================
%%% Internal tests
%%%=====================================================================

setup() ->
    {_, Pid}=nn_discovery_service:start_link(),
    Convergence_layers=["http","udp"],
    nn_discovery_service:set_cl(Convergence_layers),
    Pid.

teardown(Pid) ->
    nn_discovery_service:kill(Pid).

internal_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun get_CL/1
     ]}.


get_CL(_)->
    CL=get_cl_list(),
    [
     ?_assertEqual(CL,["http,","udp,"])
    ].
