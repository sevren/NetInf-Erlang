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
%%% @author Jon Borglund
%%% @author Kiril Goguev
%%% @author Marcus Ihlar
%%% @doc
%%% This module provides a set of functions for conversion between network
%%% layer messages and internal NetInf messages. If incoming network messages
%%% are ill formed, an error response in the corresponding network message
%%% type will be created.
%%% @end
%%% Created : 16 Oct 2012 by Thomas Nordström and Faroogh Hassan
%%%-------------------------------------------------------------------
-module(nn_http_formatting).

-include_lib("eunit/include/eunit.hrl").

-behaviour(nn_formatting).

-export([create_message/2, 
	 parse/1, 
	 create_error_message/3,
	 assemble_body/1]).

%% Test helpers
-export([set_test_state/0, 
	 clear_test_state/1]).

-define(MSGBOUNDARY, <<"--------WebKitFormBoundaryjTwy4nYi2Aj6shCW">>).

log(Level, Action, Msg) ->
    nn_logger_server:log(Level, ?MODULE, Action, Msg).

%%%========================================================================
%%% API Functions
%%%========================================================================

%%--------------------------------------------------------------------
%% @doc
%%  This funtion will try to parse the HTTP message into a NetInf message. 
%% @end
%%--------------------------------------------------------------------
-spec parse(cowboy_req:req()) -> 
		   {ok, get, binary(), binary()} |
		   {ok, publish, cowboy_req:req(), binary(), nn_proto:proto()} |
		   {error, integer(), cowboy_req:req()}.
parse(Req)->
    log(verbose, parse, "parse an HTTP request"),
    case is_netinf_req(Req) of
	{ok, get} -> 
	    handle_get(Req);
	{ok, publish} ->
	    handle_publish(Req);
	{ok, search} ->
	    handle_search(Req);
	{ok, false} ->
	    {ok, {Status, ErrorReq}} = 
		create_error_message(<<"unknown msgid">>, parse_error, Req),
	    {error, {Status, ErrorReq}}
    end.

%%--------------------------------------------------------------------
%% @doc
%%  This function will create an HTTP message from a NetInf message. 
%% @end
%%--------------------------------------------------------------------
-spec create_message(nn_proto:proto(), cowboy_req:req()) ->
			    {ok, cowboy_req:req()}.
create_message(Msg, Req) ->
    MsgId = nn_proto:get(msgid, Msg),
    case nn_proto:get(method, Msg) of
	error ->
	    create_error_message(MsgId, nn_proto:get(proto, Msg), Req);
	get ->
	    case nn_proto:get(octets, Msg) of
		undefined ->
		    create_message(netinf_resp, 
				   {MsgId, 203, 
				    nn_proto:get(proto, Msg)}, Req);
		Octets ->
		    create_message(netinf_resp, 
				   {MsgId, 200, 
				    nn_proto:get(proto, Msg), Octets}, Req)
	    end;
	publish ->
	    create_message(netinf_resp, {MsgId, 201, 
					 nn_proto:get(proto, Msg)}, Req);
	search ->
	    create_message(netinf_searchresp, {MsgId, 200, 
					       nn_proto:get(proto, Msg)}, Req)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Generates an error reply message with appropriate status code
%%  depending on error type.
%% @end
%%--------------------------------------------------------------------
-spec create_error_message(term(), atom(), cowboy_req:req()) ->
				  {ok, {integer(), cowboy_req:req()}}.
create_error_message(MsgId, Error, Req) ->
    log(verbose, create_error_message, ["MsgId: ", MsgId]),
    Status = error_to_status(Error),
    TimeStamp = nn_util:create_timestamp(),
    ResponseObject = create_response_object(MsgId, Status, TimeStamp, []),
    create_http_reply(ResponseObject, Status, Req).

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%%%====================================================================
%%% Http NetInf to Erlang NetInf
%%%====================================================================

%%%====================================================================
%%% General
%%%====================================================================
	       
is_netinf_req(Req)->
    log(verbose, is_netinf_req, 
	["determines which if any NetInf message"]),
    Wellknown = <<"/.well-known">>,
    Get = <<"/netinfproto/get">>,
    Publish = <<"/netinfproto/publish">>,
    Search = <<"/netinfproto/search">>,
	
    case cowboy_req:get(path, Req) of
	Path when Path =:= Get orelse 
		  Path =:= <<Wellknown/binary, Get/binary>> -> 	
	    {ok, get};
	Path when Path =:= Publish orelse 
		  Path =:= <<Wellknown/binary, Publish/binary>> -> 	
	    {ok, publish};
	Path when Path =:= Search orelse 
		  Path =:= <<Wellknown/binary, Search/binary>> -> 	
	    {ok, search};
	_ ->
	     log(verbose, is_netinf_req, "@ not a netinf request"),
	    {ok, false}
    end.

decode_buffer(Req) ->
    cowboy_http:urldecode(cowboy_req:get(buffer, Req)).

uri_split(Buffer, Element) ->
    case binary:match(Buffer, Element) of
	nomatch ->
	    <<>>;
	_ ->
	    [_, URIFirstSplit] = binary:split(Buffer, Element),
	    hd(binary:split(URIFirstSplit, <<"&">>))
    end.

%%%========================================================================
%%% Search 
%%%========================================================================

handle_search(Req) ->
    log(verbose, handle_search, 
	"handeling search"),
    DecodedBuffer = cowboy_req:get(buffer, Req),
    MsgId = uri_split(DecodedBuffer, <<"msgid=">>),
    Tokens = cowboy_http:urldecode(uri_split(DecodedBuffer, <<"tokens=">>)), 
    TokenList = binary:split(Tokens, <<" ">>, [global]),
    FilteredTokenList = lists:filter(fun(X) -> X =/= <<"">> end, TokenList),
    Ext = uri_split(DecodedBuffer, <<"ext=">>),
    Msg = nn_proto:new(msg, MsgId),
    case binary:match(DecodedBuffer, <<"TTL">>) of
	nomatch ->
	    {ok, nn_proto:set([{method, search}, {tokens, FilteredTokenList},
		       {proto, Ext}], Msg)};
	_ -> 
	    TTLBinary = uri_split(DecodedBuffer, <<"TTL=">>),
	    case nn_util:safe_list_to_integer(binary_to_list(TTLBinary)) of
		{error, no_integer} ->
		    create_error_message(MsgId, parse_error, Req);
		{ok, TTL} ->
		    {ok, nn_proto:set([{time_to_live, TTL},
				       {method, search}, {tokens, FilteredTokenList},
				       {proto, Ext}], Msg)}
	    end
    end.

get_results(ListOfNetInfObjects) -> 
    log(verbose, get_results, ["Extracting names from netinf objects"]),
    get_results(ListOfNetInfObjects, []).
get_results([], Results) ->
    Results;
get_results([H|TL], Results) ->
    Name = nn_proto:get(name, H),
    {ExtList} = nn_proto:get(ext, H),
    get_results(TL, [{[{"ni", Name} | ExtList]} | Results]).
  
%%%========================================================================
%%% Get 
%%%========================================================================

handle_get(Req) ->
    log(verbose, handle_get, 
	"retrieve NDO name and message ID from an HTTP request"),
    DecodedBuffer = decode_buffer(Req),
    CanonicalizedURI = uri_split(DecodedBuffer, <<"URI=">>),
    CleanURI = clean_uri(CanonicalizedURI),
    MsgId = uri_split(DecodedBuffer, <<"msgid=">>),
    Msg = nn_proto:new(msg, MsgId),
    case binary:match(DecodedBuffer, <<"TTL">>) of
	nomatch ->
	    {ok, nn_proto:set([{method, get}, {proto, CleanURI}], Msg)};
	_ ->
	    TTLBinary = uri_split(DecodedBuffer, <<"TTL=">>),
	    case nn_util:safe_list_to_integer(binary_to_list(TTLBinary)) of
		{ok, no_integer} ->
		    create_error_message(MsgId, parse_error, Req);
		{ok, TTL} ->
		    {ok, nn_proto:set([{time_to_live, TTL}, 
				       {method, get}, 
				       {proto, CleanURI}], Msg)}
	    end
    end.

clean_uri(Bin) ->
    nn_util:bin_replace(Bin, $ , $+).

%%%========================================================================
%%% Publish 
%%%========================================================================

handle_publish(Req)->
    handle_publish(Req, 
		   {undefined, undefined, undefined, undefined, nn_proto:new(undefined)}).

handle_publish({eof, _}, {MsgId, _, FullNdoFlag, Octets, NetInfMsg}) ->
    log(verbose, handle_publish, 
	"Finished parsing the http message"),
    Msg = nn_proto:new(msg, MsgId),
    case FullNdoFlag of 
		<<"true">> ->
		    ReplyMsg = nn_proto:set([
					     {method, publish},
					     {proto, NetInfMsg},
					     {octets, Octets}], Msg); 
		_ ->
		    ReplyMsg = nn_proto:set([
					     {method, publish},
					     {proto, NetInfMsg}], Msg)
	    end,
    {ok, ReplyMsg};
handle_publish({end_of_part, Req}, {MsgId, Rform, FullNdoFlag, Octets, NetInfMsg}) ->
log(verbose, handle_publish, 
		["Finished parsing a part of the http message: "]),
	    handle_publish(Req, {MsgId, Rform, FullNdoFlag, Octets, NetInfMsg});
handle_publish({headers, Header, HeaderReq}, {MsgId, Rform, FullNdoFlag, Octets, NetInfMsg}) ->
    parse_body_part(
      Header, HeaderReq, MsgId, Rform, FullNdoFlag, Octets, NetInfMsg);
handle_publish(Req, {MsgId, Rform, FullNdoFlag, Octets, NetInfMsg}) ->
    log(verbose, handle_publish, 
	["retrieve NDO message from an HTTP publish"]),
    case cowboy_req:multipart_data(Req) of
	{headers, Header, HeaderReq} ->
	   parse_body_part(
	     Header, HeaderReq, MsgId, Rform, FullNdoFlag, Octets, NetInfMsg);
	{end_of_part, EoPReq} ->
	    log(verbose, handle_publish, 
		["Finished parsing a part of the http message: "]),
	    handle_publish(EoPReq, {MsgId, Rform, FullNdoFlag, Octets, NetInfMsg});
	{eof, _} ->
	    log(verbose, handle_publish, 
		"Finished parsing the http message"),
	    Msg = nn_proto:new(msg, MsgId),
	    case FullNdoFlag of 
		<<"true">> ->
		    ReplyMsg = nn_proto:set([
					     {method, publish},
					     {proto, NetInfMsg},
					     {octets, Octets}], Msg); 
		_ ->
		    ReplyMsg = nn_proto:set([
					     {method, publish},
					     {proto, NetInfMsg}], Msg)
	    end,
	    {ok, ReplyMsg};
	{error, badarg} ->
	    log(verbose, handle_publish, "Not a multipart publish"),
	    {ok, Status, ErrorReq} = create_error_message(MsgId, 400, Req),
	    {error, Status, ErrorReq};
	_ ->
	    log(verbose, handle_publish, 
		["Something strange, in the neighbourhood, who you gonna call:~n~n~n "])	
    end.

parse_body_part(
  Header, HeaderReq, MsgId, Rform, FullNdoFlag, Octets, NetInfMsg) -> 

    case get_header_type(Header) of
	{ok, locator} ->
	    log(verbose, handle_publish, "Parsing locators"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    OldLocators = nn_proto:get(uri, NetInfMsg),
	    handle_publish(ReplyReq, {MsgId,
				      Rform,
				      FullNdoFlag,
				      Octets,
				      nn_proto:set([
						    {uri, 
						     [BodyData | OldLocators]
						    }], NetInfMsg)});
	{ok, msgid} ->
	    log(verbose, handle_publish, "Parsing MsgId"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    handle_publish(ReplyReq, {BodyData,
				      Rform,
				      FullNdoFlag,
				      Octets,
				      NetInfMsg});
	{ok, ext} ->
	    log(verbose, handle_publish, "Parsing ext"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    decode_ext(
	      BodyData, ReplyReq, MsgId, Rform, FullNdoFlag, Octets, NetInfMsg);
	{ok, uri} ->
	    log(verbose, handle_publish, "Parsing URI"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    {NewURI, CT} = get_ct(BodyData),
	    {Ext} = nn_proto:get(ext, NetInfMsg),
	    handle_publish(ReplyReq, {MsgId,
				      Rform,
				      FullNdoFlag,
				      Octets,
				      nn_proto:set([{name, NewURI},
						    {ext, {[CT|Ext]}}], NetInfMsg)});
	{ok, rform} ->
	    log(verbose, handle_publish, "Parsing Rform"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    handle_publish(ReplyReq, {MsgId,
				      BodyData,
				      FullNdoFlag,
				      Octets,
				      NetInfMsg});
	{ok, fullput} ->
	    log(verbose, handle_publish, "Parsing fullPut"),
	    {ReplyReq, BodyData} = assemble_body(HeaderReq),
	    case BodyData of
		<<"true">> ->
		    handle_publish(ReplyReq, {MsgId,
					      Rform,
					      BodyData,
					      Octets,
					      NetInfMsg});
		_ ->
		    handle_publish(ReplyReq, {MsgId,
					      Rform,
					      false,
					      <<>>,
					      NetInfMsg})
	    end;
	{ok, octets} ->
	    case FullNdoFlag of
		false ->
		    {ok, ReplyReq} = cowboy_req:multipart_skip(HeaderReq),
		    handle_publish(ReplyReq, {MsgId,
					      Rform,
					      FullNdoFlag,
					      Octets,
					      NetInfMsg});
		_ -> 
		    {ReplyReq, BodyData} = assemble_body(HeaderReq),    
		    handle_publish(ReplyReq, {MsgId,
					      Rform,
					      FullNdoFlag,
					      decode_octets(Header, BodyData),
					      NetInfMsg})
	    end;
	nomatch ->
	    log(verbose, handle_publish, ["Got strange part"]),
	    {ok, ReplyReq} = cowboy_req:multipart_skip(HeaderReq),
	    handle_publish(ReplyReq, {MsgId, 
				      Rform, 
				      FullNdoFlag, 
				      Octets, 
				      NetInfMsg})
    end.

decode_ext(
  BodyData, ReplyReq, MsgId, Rform, FullNdoFlag, Octets, NetInfMsg) ->
    {Ext} = nn_proto:get(ext, NetInfMsg),
    case json:decode(BodyData) of
	{ok, {JsonExt}} ->
	    handle_publish(ReplyReq, {MsgId,
				      Rform,
				      FullNdoFlag,
				      Octets,
				      nn_proto:set([{ext, {JsonExt ++ Ext}}], NetInfMsg)});
	_ ->
	    handle_publish(ReplyReq, {MsgId,
				      Rform,
				      FullNdoFlag,
				      Octets,
				      nn_proto:set([{ext, {Ext}}], NetInfMsg)})
    end.

decode_octets(Header, BodyData) ->
    log(verbose, decode_octets, ["Decoding octets"]),
    case proplists:get_value(
	   <<"content-transfer-encoding">>, Header) of
	<<"base64">> -> base64:mime_decode(BodyData);
	_ -> BodyData
    end.

get_header_type([])->
    nomatch;
get_header_type([{_, Head} | Tail]) -> 
    log(verbose, get_header_type, ["Header"]),
    case binary:split(Head, <<"=">>) of
	[_, <<"\"URI\"">>]->
	    {ok, uri};
	[_, <<"\"msgid\"">>] ->
	    {ok, msgid};
	[_, <<"\"ext\"">>]->
	    {ok, ext};
	[_, <<"\"rform\"">>] ->
	    {ok, rform};
	[_, <<"\"fullPut\"">>] ->
	    {ok, fullput};
	[_, <<"\"octets\"", _/binary>>] ->
	    {ok, octets};
	[_, <<"\"loc", _/binary>>] ->
	    {ok, locator};		
	_ ->
	    get_header_type(Tail)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Assembles the multipart body of Http Req   
%% @end
%%--------------------------------------------------------------------
-spec assemble_body(cowboy_req:req()) -> {cowoy_req:req(), binary()}.
assemble_body(Req) ->
    assemble_body(Req, <<>>).
assemble_body(Req, Acc) ->
    case cowboy_req:multipart_data(Req) of 
	{body, BodyBinary, BodyReq} ->
	    assemble_body(BodyReq, <<Acc/binary, BodyBinary/binary>>);
	{eof, ReplyReq} ->
	    {{eof, ReplyReq}, Acc};
	{headers, Headers, NewReq} ->
	    {{headers, Headers, NewReq}, Acc};
	{end_of_part, NewReq} ->
	    {{end_of_part, NewReq}, Acc};
	_ ->
	    {error, error}
    end.
	    
%%%====================================================================
%%% End of Http NetInf to Erlang NetInf
%%%====================================================================

%%%====================================================================
%%% Erlang NetInf to Http NetInf
%%%====================================================================

create_message(internal, {MsgId, Status, NetInf}, Req) ->
    log(verbose, create_message, 
	["Creates a response message to the get request"]),
    ListOfData = nn_proto:get([name, uri, ext, time_stamp], NetInf),
    [Name, URI, Ext, TimeStamp] = clean_list(ListOfData),
    Ct = extract_content_type(Ext),
    log(verbose, create_message, ["Ct: ", Ct]),
    Pairs = [
	     {<<"ni">>, Name}, 
	     {<<"ct">>, Ct}, 
	     {<<"metadata">>, Ext}, 
	     {<<"loc">>, URI}
	    ],
    ResponseObject = create_response_object(
		       MsgId, Status, TimeStamp, Pairs),
    create_http_reply(ResponseObject, Status, Req);

create_message(netinf_resp, {MsgId, Status, Netinf}, Req) ->
    {ok, {Status, HeadlessReq}} = 
	create_message(internal, {MsgId, Status, Netinf}, Req),
    ReplyReq = cowboy_req:set_resp_header(<<"Content-Type">>,
					    <<"application/json">>,
					     HeadlessReq),
    {ok, {Status, ReplyReq}};

create_message(netinf_resp, {MsgId, Status, NetInf, Binary}, Req) ->
    {ok, {Status, HeadlessReq}} = 
	create_message(internal, {MsgId, Status, NetInf}, Req),
    MultipartReq = cowboy_req:set_resp_header(<<"Content-Type">>,
					     list_to_binary([<<"multipart/form-data; boundary=">>,
							    ?MSGBOUNDARY]),
					     HeadlessReq),
    OldBody = cowboy_req:get(resp_body, MultipartReq),
    MsgBoundary = list_to_binary([<<"--">>, ?MSGBOUNDARY]),
    NewBody = list_to_binary([
			      MsgBoundary,
			      <<"\r\n">>,
			      <<"Content-Disposition: ">>,
			      <<"form-data; name=\"json\"\r\n">>,
			      <<"Content-Type: ">>,
			      <<"application/json\r\n\r\n">>,
			      OldBody,
			      <<"\r\n">>,
			      MsgBoundary,
			      <<"\r\n">>,
			      <<"Content-Disposition: ">>,
			      <<"form-data; name=\"octets\"\r\n">>,
			      <<"Content-Type: ">>,
			      <<"application/octet-stream\r\n\r\n">>,
			      Binary,
			      <<"\r\n">>,
			      MsgBoundary,
			      <<"--">>
			     ]),
    FullReq = cowboy_req:set_resp_body(NewBody, MultipartReq),
    {ok, {Status, FullReq}};

create_message(netinf_searchresp, {MsgId, Status, Msg}, Req) ->
    Results = get_results(Msg),
    log(verbose, create_message, 
	["Creates a response message to the search request", Results]),
    TimeStamp = nn_util:create_timestamp(),
    ResponseObject = create_response_object(
		       MsgId, Status, TimeStamp, [{<<"results">>, Results}]),
    create_http_reply(ResponseObject, Status, Req).

create_response_object(MsgId, Status, TimeStamp, KVList) ->
    {
      [
       {<<"NetInf">>, <<"v0.1a">>},
       {<<"msgid">>, MsgId},
       {<<"status">>, Status},
       {<<"ts">>, TimeStamp}
      ] ++ KVList
    }.

create_http_reply(Object, Status, Req) ->
    {ok, Body} = json:encode(Object),
    HeadlessReq = cowboy_req:set_resp_body(Body, Req),
    ReplyReq = cowboy_req:set_resp_header(<<"Content-Type">>,
					    <<"application/json">>,
					     HeadlessReq),
    {ok, {Status, ReplyReq}}.

clean_list([]) ->
    [];
clean_list([undefined | Tl]) ->
    [<<"">> | clean_list(Tl)];
clean_list([Hd | Tl]) ->
    [Hd | clean_list(Tl)].

get_ct(Header) ->
    case binary:match(Header, <<"ct=">>) of
	nomatch ->
	    {Header, {<<"ct">>, <<"">>}};
	_ ->
	    [RepHeader, CT] = binary:split(Header, <<"?">>),
	    [<<"ct">>, CTValue] = binary:split(CT, <<"=">>),
	    {RepHeader, {<<"ct">>, CTValue}}
    end.

extract_content_type(Ext) ->
    {ExtList} = Ext,
    proplists:get_value(<<"ct">>, ExtList, <<"">>).

error_to_status(no_match) ->
    404;
error_to_status(invalid_hash) ->
    406;
error_to_status(_) ->
    400.

%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.


meckd_multipart_data([]) ->
    {eof, []};
meckd_multipart_data([{Atom, Value}|Tl]) ->
    {Atom, Value, Tl}.

set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log,
		fun(_, _, _, Msg) -> Msg end),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, multipart_skip, fun([_|Tl]) -> Tl end),
    meck:expect(cowboy_req, multipart_data, fun meckd_multipart_data/1),
    meck:expect(cowboy_req, get, fun(_, Req) -> Req end),
    meck:new(cowboy_http),
    meck:expect(cowboy_http, urldecode, fun(Req) -> Req end),
    meck:expect(cowboy_req, qs, fun(_) -> ok end),
    meck:expect(cowboy_req, set_resp_body, fun(Reply, _) -> Reply end),
    meck:expect(cowboy_req, set_resp_header, fun(_, _, Reply) -> Reply end).

clear_test_state(_)->
    meck:unload(nn_logger_server),
    meck:unload(cowboy_req),
    meck:unload(cowboy_http).

run_test_() ->
    {foreach,
     fun set_test_state/0,
     fun clear_test_state/1,
     [
      fun test_handle_publish/1,
      fun test_handle_search/1,
      fun test_handle_get/1,
      fun test_create_message/1
     ]}.


test_handle_publish(_) ->
    Req = [{headers, [{unused, <<"test=\"URI\"">>}]},
	   {body, <<"name">>},
	   {headers, [{unused, <<"test=\"msgid\"">>}]},
	   {body, <<"message id">>},
	   {headers, [{unused, <<"test=\"ext\"">>}]},
	   {body, <<"{\"id\": 5,\"meta\":{\"tag\":[1, 2, 3]}}">>},
	   {headers, [{unused, <<"test=\"rform\"">>}]},
	   {body, <<"json">>},
	   {headers, [{unused, <<"test=\"loc1\"">>}]},
	   {body, <<"locator 1">>},
	   {headers, [{unused, <<"test=\"loc2\"">>}]},
	   {body, <<"locator 2">>}],
    Result = handle_publish(Req),
    {ok, ExpectedJson} = 
	json:decode(<<"{\"id\": 5,\"meta\":{\"tag\":[1, 2, 3]}, \"ct\":\"\"}">>),
    ExpectedProto = nn_proto:new(<<"name">>, 
				 [<<"locator 2">>, <<"locator 1">>],
				 ExpectedJson,
				 undefined %"2012-12-12T12:12:12+00:00"
				),
    [
     ?_assertMatch({ok, publish, [], <<"message id">>, _}, Result),
     ?_assertEqual(ExpectedProto, element(5, Result))
    ].

test_create_message(_) ->
    {ok, InputJson} = 
	json:decode(<<"{\"ct\": \"image/png\",\"meta\":{\"tag\":[1, 2, 3]}}">>),
    InputProto = nn_proto:new(<<"name">>, 
				 [<<"locator 2">>, <<"locator 1">>],
				 InputJson,
				 undefined %"2012-12-12T12:12:12+00:00"
				),
    Result = create_message(netinf_resp,
			    {<<"identifier">>, 203, InputProto}, unused),
   {ok, Data} = json:decode("{
   \"NetInf\":\"v0.1a\",
   \"msgid\":\"identifier\",
   \"status\":203,
   \"ni\":\"name\",
   \"ts\":\"\",
   \"ct\":\"image/png\",
   \"metadata\":{
      \"ct\":\"image/png\",
      \"meta\":{
         \"tag\":[
            1,
            2,
            3
         ]
      }
   },
   \"loc\":[
      \"locator 2\",
      \"locator 1\"
   ]
   }"),

    {ok, ExpectedJson} = json:encode(Data), 
    [
     ?_assertMatch({ok, {_, _}}, Result),
     ?_assertMatch({ok, {203, _}}, Result),
     ?_assertMatch({ok, {203, ExpectedJson}}, Result)
    ].

test_handle_get(_) ->
    Req = <<"sdfjkghmsgid=identifier&sdklfghURI=name&sghsdlöfhkg&sjkdgf">>,
    Result = handle_get(Req),
    [
     ?_assertMatch({ok, get, _, _}, Result),
     ?_assertEqual(<<"name">>, element(3, Result)),
     ?_assertEqual(<<"identifier">>, element(4, Result))
    ].

test_handle_search(_) ->
    Req = <<"msgid=12&tokens=name&ext=[]">>,
    Result = handle_search(Req),
    [
     ?_assertMatch({ok, search, _, _, _}, Result),
     ?_assertEqual([<<"name">>], element(4, Result)),
     ?_assertEqual(<<"[]">>, element(5, Result))
    ].
