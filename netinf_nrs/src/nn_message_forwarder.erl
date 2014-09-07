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
%%% @author Alexander Lindholm
%%% @doc
%%% Forwards a request to the correct convergence layer-forwarder if the 
%%% object is not found in the local NRS.
%%% @end
%%% Created :  19 Nov 2012 by Alex
%%%-------------------------------------------------------------------
-module(nn_message_forwarder).

-export([forward/1]).
%%%=====================================================================
%%% API
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc 
%%  Forwards the request to the correct convergence layer-forwarder
%%  Returns {ok, forwarded, X} where X is the number of NRS'es forwarded to
%%  or {ok, no_match} if the requested operation isn't valid.
%% @end
%%----------------------------------------------------------------------
-spec forward(nn_proto:proto_msg()) -> 
			 {ok, forwarded, integer()} | {ok, no_match}.
forward(Msg) ->
    nn_logger_server:log(verbose, ?MODULE, forward,
			 ""),
    Method = nn_proto:get(method, Msg),
    forward(Method, Msg).

forward(get, Msg) ->
    Data = nn_proto:get(proto, Msg),
    MsgId = nn_proto:get(msgid, Msg),
    case nn_proto:get(time_to_live, Msg) of
	undefined ->
	    TTL = 5;
	TTL ->
	    ok
    end,
    forward(get, binary_to_list(Data), binary_to_list(MsgId),
	    integer_to_list(TTL));
forward(search, Msg) ->
    Data = nn_proto:get(tokens, Msg),
    MsgId = nn_proto:get(msgid, Msg),
    case nn_proto:get(time_to_live, Msg) of
	undefined ->
	    TTL = 5;
	TTL ->
	    ok
    end,
    forward(search, Data, binary_to_list(MsgId), integer_to_list(TTL)).
    

%%%=====================================================================
%%% Internal functions 
%%%=====================================================================
forward(get, URI, MsgId, TTL) ->
    nn_logger_server:log(verbose, ?MODULE, forward,
			 "get"),
    DestList = nn_discovery_client:get_ip_list(),
    forward_req(get, URI, MsgId, TTL, DestList, 0);
forward(search, Tokens, MsgId, TTL) ->
    nn_logger_server:log(verbose, ?MODULE, forward,
			 "search"),
    DestList = nn_discovery_client:get_ip_list(),
    forward_req(search, Tokens, MsgId, TTL, DestList, 0);
forward(_, _, _, _) ->
nn_logger_server:log(verbose, ?MODULE, forward_req_any_undef,
			 "forward-ALL-undefined"),
    {ok, no_match}.

forward_req(get, Msg, MsgId, _, undefined, _) ->
    nn_logger_server:log(verbose, ?MODULE, forward_req_get_udp,
			 "udp-get"),
    udp_forwarder(get, Msg, MsgId, 0, undefined, 0),
    {ok, forwarded, 1};

forward_req(search, Tokens, MsgId, _, undefined, _) ->
 nn_logger_server:log(verbose, ?MODULE, forward_req_search_udp,
			 "udp-search"),
    udp_forwarder(search, Tokens, MsgId, 0, undefined, 0),
     {ok, forwarded, 1};

forward_req(_, _, _, _, undefined, _) ->
     nn_logger_server:log(verbose, ?MODULE, forward_req_any_undef,
			 "forward-undefined"),
    {ok, no_match};
forward_req(_, _, _, _, [], X) ->
     nn_logger_server:log(verbose, ?MODULE, forward_req_any_any,
			 "forward-any"),
    {ok, forwarded, X};
forward_req(get, Msg, MsgId, TTL, [{http, DestList}|TL], X) ->
    nn_logger_server:log(verbose, ?MODULE, forward_req_http,
			 "forward_req_http-get"),
    Y = http_forwarder(get, Msg, MsgId, TTL, DestList, 0),
    forward_req(get, Msg, MsgId, TTL, TL, X+Y);
forward_req(search, Msg, MsgId, TTL, [{http, DestList}|TL], X) ->
     nn_logger_server:log(verbose, ?MODULE, forward_req_search_http,
			 "http-search"),
    Y = http_forwarder(search, Msg, MsgId, TTL, DestList, 0),
    forward_req(search, Msg, MsgId, TTL, TL, X+Y).





http_forwarder(_, _, _, _, [], X) ->
    X;
http_forwarder(Req, Msg, MsgId, TTL, [Dest|TL], X) ->
    {ok, Pid} = nn_http_forwarder:spawn(),
    IP = inet_parse:ntoa(Dest),
    nn_http_forwarder:forward_request(Pid, Req, Msg, MsgId, TTL, IP),
    http_forwarder(Req, Msg, MsgId, TTL, TL, X+1);
http_forwarder(_, _, _, _, _, _) ->
    undefined.

udp_forwarder(Method, Msg, MsgId, _, _, _)->
    nn_logger_server:log(verbose, ?MODULE, udp_forward,
			 "udp_forward_get"),
    {ok, Pid} = nn_udp_forwarder:spawn(),
    nn_udp_forwarder:forward_request(Pid, Method, Msg, MsgId).
    
    




    
