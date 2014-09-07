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
%%% @author Daniele Bacarella
%%% @doc
%%% This module defines the funcions that allow for subscribing to a
%%% video stream
%%% @end
%%% Created :  6 Dec 2012 by Alex <alex@Alexcomp>
%%%-------------------------------------------------------------------
-module(nn_subscribe).

%% API
-export([
	 subscribe/2, 
	 check_locators/2,
	 generate_publish/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Subscribe to a stream where URI = name of stream and IP = publisher
%%  of stream.
%% 
%% @end
%%--------------------------------------------------------------------
-spec subscribe(URI :: string(), IP :: string()) ->
		       {ok, string()} | {error, string()}.
subscribe(URI, IP) ->
    URL = "http://" ++ IP ++ ":9999/netinfproto/get",
    GetResponse = generate_get(URI, unique_id(), [], URL),
    nn_logger_server:log(verbose, ?MODULE, subscribe,
			 ["Getresponse"]),
    case  get_part(<<"status">>, GetResponse) >= 400 of
	true ->
	    {error, "Stream does not exist"};
	false ->
	    PubURL = "http://" ++ IP ++ ":9999/netinfproto/publish",
	    MyLoc = "http://" ++ 
		nn_discovery_service:get_ipaddr() ++ 
		":9999/netinfproto",
	    PubResponse = generate_publish(PubURL, 
					    [{'URI', URI}, 
					     {msgid, unique_id()}, 
					     {ext, "{\"metadata\": \"stream\"}"}, 
					     {loc, MyLoc},
					     {metadata, "stream"}], []),
	    nn_logger_server:log(verbose, ?MODULE, subscribe, ["publish response"]),
	    Locs = get_part(<<"loc">>, PubResponse),
	    FixedLocs = fix_locations(Locs, URI),
	    {ok, FixedLocs}
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Get locators for the stream
%% @end
%%--------------------------------------------------------------------
-spec check_locators(URI :: string(), IP :: string()) ->
		       {ok, string()} | {error, string()}.
check_locators(URI, IP) ->
    URL = "http://" ++ IP ++ ":9999/netinfproto/get",
    GetResponse = generate_get(URI, unique_id(), [], URL),
    case  get_part(<<"status">>, GetResponse) >= 400 of
	true ->
	    {error, "Stream does not exist"};
	false ->
	    Locs = get_part(<<"loc">>, GetResponse),
	    FixedLocs = fix_locations(Locs, URI),
	    {ok, FixedLocs}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fix_locations(Loc, URI) ->
    fix_locations(Loc, URI, []).

fix_locations([], _, FixedLocs) ->
    FixedLocs;
fix_locations([<<"">>|T], URI, FixedLocs) ->
    fix_locations(T, URI, FixedLocs);
fix_locations([H|T], URI, FixedLocs) ->
    %LocString = "http://" ++ binary_to_list(H) ++ ":8078/octets/" 
%	++ binary_to_list(cowboy_http:urlencode(list_to_binary(URI))),
    LocString = binary_to_list(H) ++ "/get",
    fix_locations(T, URI, [LocString|FixedLocs]). 

unique_id() ->
    {_, _, Micros} = now(),
    integer_to_list(Micros).
get_part(Key, Response)->
    proplists:get_value(Key, Response).
    
generate_get(URI, MsgID, Ext, GetURL) ->
    {ok, {_, HList, ResponseBody}} = httpc:request(post, 
		       {GetURL, 
			[], 
			"application/x-www-form-urlencoded",
			"URI="++ URI ++ "&msgid="++ MsgID ++ "&ext=" ++ Ext
		       }, [], []),
    
    case proplists:get_value("content-type", HList) of
	"multipart/form-data; boundary="++Boundary ->
	    nn_util:decode(Boundary, ResponseBody);
	_ ->
	    nn_util:decode(ResponseBody)
    end.

generate_publish(URL, Publish, Data) ->
    Boundary = "------WebKitFormBoundaryjTwy4nYi2Aj6shCW",
    Body = format_multipart_formdata(
	     Boundary, Publish, Data),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(length(Body))}],
    
    case httpc:request(post, 
		       {URL, Headers, ContentType, Body}, [], []) of
	{ok, {_, [{_, <<"multipart/form-data;", Rest/binary>>}], ResponseBody}} ->
	    nn_util:decode(Rest, ResponseBody);
	{ok, {_, _, ResponseBody}} ->
	    nn_util:decode(ResponseBody)
    end.

format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(
		   fun({FieldName, FieldContent}) ->
			  format_one_part(FieldName, FieldContent, Boundary)
		   end, Fields),
    FieldParts2 = lists:append(FieldParts),

    FileParts = lists:map(
		  fun({FieldName, FileContent}) ->
			  format_one_part(FieldName, FileContent, Boundary)
		  end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").

format_one_part(octets, Binary, Boundary) ->
    [lists:concat(["--", Boundary]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(octets), "\""]),
     lists:concat([
		   "Content-Type: ", "application/octet-stream"]),
     "",
     binary_to_list(Binary)];
format_one_part(FieldName, FieldContent, Boundary) ->
    [lists:concat(["--", Boundary]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(FieldName),"\""
		  ]),
     "",
     FieldContent].
