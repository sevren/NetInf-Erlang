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
%%% This module parses a NetInf message and determines if it is a 
%%% Get, Publish or Search message. It also creates a UDP message
%%% out of a NetInf message.   
%%% @end
%%% Created :  3 Dec 2012 by Thomas
%%--------------------------------------------------------------------

-module(nn_udp_formatting).


-behaviour(nn_formatting).

-export([create_message/2, parse/1]).

log(Level, Action, Msg) ->
    nn_logger_server:log(Level, ?MODULE, Action, Msg).

%%--------------------------------------------------------------------
%% @doc
%% Parses a NetInf message and determines the message type. 
%% @end
%%--------------------------------------------------------------------
-spec parse(nn_proto:proto()) -> {ok, nn_proto:proto(), nn_proto:proto()}|
				 {error, error}.
parse(Msg) ->
    case json:decode(Msg) of
	{ok, {JsonList}} ->
	    log(verbose, parse, "parsed ok! Json is valid"),
	    handle_request(proplists:get_value(<<"msgType">>, JsonList), JsonList);
	_ ->
	    log(verbose, parse, "error- json is invalid"),
	    create_error_message()
    end.


%%--------------------------------------------------------------------
%% @doc
%%  This function will create a UDP message from a NetInf message. 
%% @end
%%--------------------------------------------------------------------
create_message(Msg, _) ->
    MsgId = nn_proto:get(msgid, Msg),
    case nn_proto:get(method, Msg) of
	error ->
	    create_error_message();
	get ->
	    case nn_proto:get(octets, Msg) of
		undefined ->
		    create_udpmessage(netinf_resp, 
				   {MsgId, <<"GET-RESP">>, 
				    nn_proto:get(proto, Msg)});
		_ ->
		    create_error_message()
	    end;
	publish ->
	    create_udpmessage(netinf_resp, {MsgId, <<"PUBLISH-RESP">>, 
					 nn_proto:get(proto, Msg)});
	search ->
	    create_udpmessage(netinf_searchresp, {MsgId, <<"SEARCH-RESP">>, 
					       nn_proto:get(proto, Msg)})
    end.


handle_request(<<"GET">>, MsgList)->
    NewMsg = nn_proto:new(msg, proplists:get_value(<<"msgId">>, MsgList)),
    URI = proplists:get_value(<<"uri">>, MsgList, <<>>),
    {ok, nn_proto:set([{method, get}, {proto, URI}], NewMsg)};
handle_request(<<"GET-RESP">>, MsgList)->
     log(verbose, handle_request, 
	"handles the udp get response message"),
    NewMsg = nn_proto:new(msg, proplists:get_value(<<"msgId">>, MsgList)),
    URI = proplists:get_value(<<"uri">>, MsgList, <<>>),
    NetInfObject = nn_proto:set(
		     [{uri, proplists:get_value(<<"locators">>, MsgList)},
		      {time_stamp, nn_util:create_timestamp()}
		     ], nn_proto:new(URI)),
    {ok, nn_proto:set([{method, get_resp}, 

		       {proto, NetInfObject}
		      ], NewMsg)};
handle_request(<<"PUBLISH">>, MsgList) ->
    NewMsg = nn_proto:new(msg, proplists:get_value(<<"msgId">>, MsgList)),
    {URI, CT} = get_ct(proplists:get_value(<<"uri">>, MsgList, <<>>)),
    NewNetinf = nn_proto:new(URI),
    Locators = proplists:get_value(<<"locators">>, MsgList, <<>>),
    {Ext} = proplists:get_value(<<"ext">>, MsgList, {[]}), 
    NetinfProto = nn_proto:set([{uri, Locators},
				{ext, {[CT|Ext]}}], NewNetinf),  
    {ok, nn_proto:set([{method, publish},
		      {proto, NetinfProto}],
		     NewMsg)};
handle_request(<<"SEARCH">>, MsgList) ->
    NewMsg = nn_proto:new(msg, proplists:get_value(<<"msgId">>, MsgList)),
    Tokens = proplists:get_value(<<"tokens">>, MsgList),
    Ext = proplists:get_value(<<"ext">>, MsgList),
    {ok, nn_proto:set([{method, search},
		     {tokens, Tokens},
		     {proto, Ext}], NewMsg)};
handle_request(<<"SEARCH-RESP">>, MsgList) ->
    log(verbose, handle_request,["SEARCH-RESP and MsgList is ", MsgList]),
    NewMsg = nn_proto:new(msg, proplists:get_value(<<"msgId">>, MsgList)),
    Results = proplists:get_value(<<"results">>, MsgList),
    ProtoResults = [protofy(X) || X <- Results],
    {ok, nn_proto:set([{method, search_resp},
		     {proto, ProtoResults}], NewMsg)}.
    
    

create_error_message() ->
    {error, error}.


create_udpmessage(netinf_resp, {MsgId, MsgType, NetInf}) ->
    log(verbose, create_message, 
	["Creates a response message request ", MsgType]),
    Name = nn_proto:get([name], NetInf),
    URI = nn_proto:get([uri], NetInf),
   % Ext = nn_proto:get([ext], NetInf),
    Pairs = [
	     {<<"ni">>, Name}, 
	     {<<"locators">>, URI}
	    ],
    ResponseObject = create_response_object(
		       MsgId, MsgType, Pairs),
    create_udp_reply(ResponseObject);

create_udpmessage(netinf_searchresp, {MsgId, MsgType, Msg}) ->
    Results = get_results(Msg),
    log(verbose, create_message, 
	["Creates a response message to the search request", Results]),
    ResponseObject = create_response_object(
		       MsgId, MsgType,  [{<<"results">>, Results}]),
    create_udp_reply(ResponseObject).

get_results(ListOfNetInfObjects) -> 
    log(verbose, get_results, ["Extracting names from netinf objects: ",
			       ListOfNetInfObjects]),
    get_results(ListOfNetInfObjects, []).
get_results([], Results) ->
    Results;
get_results([H|TL], Results) ->
    Name = nn_proto:get(name, H),
    {ExtList} = nn_proto:get(ext, H),
    get_results(TL, [{[{"ni", Name} | ExtList]} | Results]).



get_ct(Header) ->
    case binary:match(Header, <<"ct=">>) of
	nomatch ->
	    {Header, {<<"ct">>, <<"">>}};
	_ ->
	    [RepHeader, CT] = binary:split(Header, <<"?">>),
	    [<<"ct">>, CTValue] = binary:split(CT, <<"=">>),
	    {RepHeader, {<<"ct">>, CTValue}}
    end.



create_response_object(MsgId, MsgType, KVList) ->
    {
      [
       {<<"version">>, <<"NetInfUDP/1.0">>},
       {<<"msgType">>, MsgType },
       {<<"msgId">>, MsgId}
      ] ++ KVList
    }.

create_udp_reply(Object) ->
    {ok, EncodedMsg} = json:encode(Object),
    EncodedMsg.

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
    
