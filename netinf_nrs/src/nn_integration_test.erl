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
%%%------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Jon Borglund
%%% @author Alexander Lindholm
%%% @author Marcus Ihlar
%%% @doc
%%%  System test for the netinf_nrs application. Tests the system via
%%%  its http interface. The tests check if the system conforms to the
%%%  NetInf specification as of ...
%%%  Todo: tests for search functionality
%%% @end
%%% Created : 16 Oct 2012 by Alexander <alex_thespr@yer.com>
%%%-------------------------------------------------------------------
-module(nn_integration_test).
-include_lib("eunit/include/eunit.hrl").

-define(BASEURL, "http://localhost:9999/netinfproto").

-define(GETURL, ?BASEURL "/get").
-define(PUBLISHURL, ?BASEURL "/publish").
-define(SEARCHURL, ?BASEURL  "/search").

-define(GENERIC_LOCATOR, "http://host/.well-known/content").

%%%====================================================================
%%% Test functions
%%%====================================================================

setup() ->
    timer:sleep(300),
    netinf_nrs:start(),
%    nn_storage:flush(),
    meck:new(nn_message_forwarder),
    meck:expect(nn_message_forwarder, forward,
		fun(_) -> {ok, no_match} end).

teardown(_) ->
    meck:unload(nn_message_forwarder),
    netinf_nrs:stop().
   
nn_integration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (_) -> 
        [ 
          get404([]),
	  simple_get([]),
          simple_pub([]),
          search([]),
          empty_search([]),
          merge([]),
          metadata_merge([]),
          publish_meta([]), 
          many_pubs([]),
          fullput_pub([]),
          fullput_get([]),
          fullput_octets_before_fullput_flag([]),
          fullput_bogus_hash([]),
          fullput_ommitted_flag([]),
          fullput_collision([]),
          fullput_collision_bogus_hash([]),
          test_path([]),
          test_content_transfer([]),
          test_content_put([]),
          test_hash_truncation([]),
	  test_multiple_requests()
        ]
     end
    }.


get404(_)->    
    Msg = generate_get(
	    "ni:///sha-256;niASDFgIdj2", "msgid123", "ext", ?GETURL),
    [
     ?_assertEqual(404, proplists:get_value(<<"status">>, Msg)),
     ?_assertEqual(<<"msgid123">>, proplists:get_value(<<"msgid">>, Msg))
    ].

search(_)->
    Name1 = "ni:///sha-256;MTBAR6FDmgLRj0nnu5YqAda-ZkHqin5bisykDP0wA9c",
    MetaKey1 = "meta_key1", Token1 = "test_token1",
    generate_publish([ 
		       {'URI', Name1}, 
		       {msgid, unique_id()},
		       {loc, ?GENERIC_LOCATOR},
		       {ext, 
			"{\"meta\": {\""++ MetaKey1 ++"\" : \""++ Token1 ++  "\" }}" 
		       }		       
		     ], []), 		      
    S1MsgId = [$- | unique_id()],
    SearchR1 = generate_search(S1MsgId, Token1, []),
    [{Results1}] = proplists:get_value(<<"results">>, SearchR1),
    {Meta} = proplists:get_value(<<"meta">>, Results1),

    [ 
      ?_assertNotEqual(
	 undefined, proplists:get_value(<<"NetInf">>, SearchR1)),
      ?_assertNotEqual(undefined, proplists:get_value(<<"ts">>, SearchR1)),
      ?_assertEqual(list_to_binary(S1MsgId),
		   proplists:get_value(<<"msgid">>, SearchR1)),
      ?_assertEqual(
	 list_to_binary(Name1), proplists:get_value(<<"ni">>, Results1)),
      ?_assertEqual(
	 list_to_binary(Token1), 
	 proplists:get_value(list_to_binary(MetaKey1), Meta)),
      ?_assertEqual(
	"application/json", proplists:get_value("content-type", SearchR1))
    ].

empty_search(_)->
    Name1 = "ni:///sha-256;MTBAR6FDmgLRj0nnu5YqAda-ZkHqin5bisykDP0wA9c",
    MetaKey1 = "meta_key1", Token1 = "test_token1",
    generate_publish([ 
		       {'URI', Name1}, 
		       {msgid, unique_id()},
               {loc, ?GENERIC_LOCATOR},
		       {ext, 
			"{\"meta\": {\""++ MetaKey1 ++"\" : \""++ Token1 ++  "\" }}" 
		       }		       
		     ], []),
    S1MsgId = [$- | unique_id()],
    SearchR1 = generate_search(S1MsgId, [], []),

    [ 
      ?_assertNotEqual(
	 undefined, proplists:get_value(<<"NetInf">>, SearchR1)),
      ?_assertNotEqual(undefined, proplists:get_value(<<"ts">>, SearchR1)),
      ?_assertEqual(list_to_binary(S1MsgId),
		   proplists:get_value(<<"msgid">>, SearchR1)),
      ?_assertEqual(
	 404, 
	 proplists:get_value(<<"status">>, SearchR1))      
    ].


simple_get(_)->
    Name = "ni:///sha-256;MTBAR6FDmgLRj0nnu5YqAda-ZkHqin5bisykDP0wA9c",
    Loc1 = ?GENERIC_LOCATOR,
    TS = "2010-01-01T20:20:20+00:00",
    Ext = [{<<"ct">>, <<"text/plain">>}],
    NetInfObj = nn_proto:new(list_to_binary(Name), [Loc1], {Ext}, TS),    
    nn_storage:publish(NetInfObj),
    Msgid = unique_id(),
    Get1 = generate_get(Name, Msgid, "ext", ?GETURL),
    RegExpTimestamp = 
	"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\+|-)\\d{2}:\\d{2}$",
    Timestamp = proplists:get_value(<<"ts">>, Get1),
    
    [
     ?_assertEqual(203, proplists:get_value(<<"status">>, Get1)),
     ?_assertEqual(list_to_binary(Msgid),
		   proplists:get_value(<<"msgid">>, Get1)),
     ?_assertEqual(list_to_binary(Name), 
		   proplists:get_value(<<"ni">>, Get1)),
     ?_assertNotEqual(undefined, Timestamp),
     ?_assertMatch({match, _}, re:run(Timestamp, RegExpTimestamp, [])),
     ?_assertEqual(<<"text/plain">>, proplists:get_value(<<"ct">>, Get1)),
     ?_assertEqual([Loc1],
		   proplists:get_value(<<"loc">>, Get1)),
      ?_assertEqual(
	"application/json", proplists:get_value("content-type", Get1))
    ].

simple_pub(_) ->
    Ct = "text/plain",
    Name = 
	"ni:///sha-256;MTBAR6FDmgLRj0nnu5YqAda-ZkHqin5bisykDP0wA9c?ct=" ++ Ct,
    Loc1 = ?GENERIC_LOCATOR,
    Loc2 = ?GENERIC_LOCATOR "/CEO.txt",
    Ext = "{\"meta\": {\"publish\": \"integration_test\"}}",
    MessageId = unique_id(),
    
    Pub1 = generate_publish([
			     {'URI', Name}, 
			     {msgid, MessageId}, 
			     {ext, Ext},  
			     {loc1, Loc1}, 
			     {loc2, Loc2}
			    ], []),
    RegExpTimestamp = 
	"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\+|-)\\d{2}:\\d{2}$",
    Timestamp = proplists:get_value(<<"ts">>, Pub1),
    [
     
     ?_assertNotEqual(undefined, proplists:get_value(<<"NetInf">>, Pub1)),
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pub1)),
     ?_assertEqual(list_to_binary(MessageId), 
		   proplists:get_value(<<"msgid">>, Pub1)),
     ?_assertEqual(list_to_binary(Ct), proplists:get_value(<<"ct">>, Pub1)),
     ?_assertNotEqual(undefined, Timestamp),
     ?_assertMatch({match, _}, re:run(Timestamp, RegExpTimestamp, [])),
     ?_assertEqual(
	"application/json", proplists:get_value("content-type", Pub1))
    ].

merge(_) ->
    Loc1 = ?GENERIC_LOCATOR "/path1",
    Bloc1 = list_to_binary(Loc1),
    Loc2 = ?GENERIC_LOCATOR "/path2",
    Bloc2 = list_to_binary(Loc2),
    Loc3 = ?GENERIC_LOCATOR "/path3",
    Bloc3 = list_to_binary(Loc3),
    Pr1 = generate_publish(
	    [{'URI', "ni:///sha-256;niasNIdj2?ct=text/plain"}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {loc1, Loc1}
	    ], []),
    Pr2 = generate_publish(
	    [{'URI', "ni:///sha-256;niasNIdj2?ct=text/plain"}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {loc1, Loc1},
	     {loc2, Loc2}
	    ], []),
    Pr3 = generate_publish(
	    [{'URI', "ni:///sha-256;niasNIdj2?ct=text/plain"}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"},  
	     {loc1, Loc1},
	     {loc2, Loc2}
	    ], []),
   Pr4 = generate_publish(
	   [{'URI', "ni:///sha-256;niasNIdj2?ct=text/plain"}, 
	    {msgid, unique_id()}, 
	    {ext, "{\"ig\": \"apa\"}"}, 
	    {loc1, Loc3},
	    {loc2, Loc3}
	   ], []),
    Pr1sorted =  lists:sort(proplists:get_value(<<"loc">>, Pr1)),
    Pr2sorted =  lists:sort(proplists:get_value(<<"loc">>, Pr2)),
    Pr3sorted =  lists:sort(proplists:get_value(<<"loc">>, Pr3)),
    Pr4sorted =  lists:sort(proplists:get_value(<<"loc">>, Pr4)),
    [
     ?_assertEqual(lists:sort([Bloc1]), Pr1sorted),
     ?_assertEqual(lists:sort([Bloc1, Bloc2]), Pr2sorted),
     ?_assertEqual(lists:sort([Bloc1, Bloc2]), Pr3sorted),
     ?_assertEqual(lists:sort([Bloc1, Bloc2, Bloc3]), Pr4sorted)
    ].

many_pubs(_) ->
    do_n_pubs(20),
    Name = "ni:///sha-256;niasNIdi72",
    Ct = "?ct=text/plain",
    Pr =  generate_publish(
	    [
	     {'URI', Name ++ Ct}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {loc1, ?GENERIC_LOCATOR "/loc1"},
	     {loc2, ?GENERIC_LOCATOR "/loc2"}
	    ], []),
    do_n_pubs(20),
    Gr = generate_get(
	   "ni:///sha-256;niasNIdi72", unique_id(), [], ?GETURL),   
    [
     ?_assertNotEqual(proplists:get_value(<<"msgid">>, Pr), 
		      proplists:get_value(<<"msgid">>, Gr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Gr)),
     ?_assertEqual(lists:sort(proplists:get_value(<<"loc">>, Pr)), 
		   [list_to_binary(?GENERIC_LOCATOR "/loc1"), 
		    list_to_binary(?GENERIC_LOCATOR "/loc2")]),
     ?_assertEqual(lists:sort(proplists:get_value(<<"loc">>, Gr)), 
		   [list_to_binary(?GENERIC_LOCATOR "/loc1"), 
		    list_to_binary(?GENERIC_LOCATOR "/loc2")])
    ].

fullput_pub(_) ->
    Binary = nn_util:n_byte_binary(100),
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    Pr =  generate_publish(
	    [
	     {'URI', Name}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {fullPut, "true"},
	     {loc1, ?GENERIC_LOCATOR "/loc1"},
	     {loc2, ?GENERIC_LOCATOR "/loc2"}
	    ], [{octets, Binary}]),
    [
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr))
    ].

fullput_get(_) ->     
    Binary = <<"fullputget">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    Pmsgid = unique_id(),
    Pr = generate_publish(
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, Pmsgid}, 
	     {ext, ""},
	     {fullPut, "true"}
	    ], [{octets, Binary}]),
    timer:sleep(50),

    Gmsgid = unique_id(),    
    Gr = generate_get(Name, Gmsgid, "ext", ?GETURL),    
    [
     ?_assertEqual(
	list_to_binary(Pmsgid), proplists:get_value(<<"msgid">>, Pr)),
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr)),
     ?_assertEqual(
	list_to_binary(Gmsgid), proplists:get_value(<<"msgid">>, Gr)),
     ?_assertEqual(200, proplists:get_value(<<"status">>, Gr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Gr)),
     ?_assertEqual(list_to_binary(Ct), proplists:get_value(<<"ct">>, Gr)),
     ?_assertEqual(Binary, proplists:get_value(<<"octets">>, Gr))
    ].

fullput_octets_before_fullput_flag(_) ->     
    Binary = <<"fullputget">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    Pmsgid = unique_id(),
    Pr = generate_publish( [{octets, Binary}],
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, Pmsgid}, 
	     {ext, ""},
	     {fullPut, "true"}
	    ]),
    timer:sleep(50),

    Gmsgid = unique_id(),    
    Gr = generate_get(Name, Gmsgid, "ext", ?GETURL),    
    [
     ?_assertEqual(
	list_to_binary(Pmsgid), proplists:get_value(<<"msgid">>, Pr)),
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr)),
     ?_assertEqual(
	list_to_binary(Gmsgid), proplists:get_value(<<"msgid">>, Gr)),
     ?_assertEqual(200, proplists:get_value(<<"status">>, Gr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Gr)),
     ?_assertEqual(list_to_binary(Ct), proplists:get_value(<<"ct">>, Gr)),
     ?_assertEqual(Binary, proplists:get_value(<<"octets">>, Gr))
    ].
   
fullput_bogus_hash(_)->  
    Binary = <<"fullputget_bogus">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, <<"test data">>))),
    Name = "ni:///sha-256;" ++ Hash,
    Pmsgid = unique_id(),
    Pr = generate_publish(
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, Pmsgid},
	     {ext, ""},
	     {fullPut, "true"}
	    ], [{octets, Binary}]),
    timer:sleep(50),

    Gmsgid = unique_id(),    
    Gr = generate_get(Name, Gmsgid, "ext", ?GETURL), 
    [
     ?_assertEqual(
	list_to_binary(Pmsgid), proplists:get_value(<<"msgid">>, Pr)),
     ?_assertEqual(
	406, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(
	list_to_binary(Gmsgid), proplists:get_value(<<"msgid">>, Gr)),
     ?_assertEqual(404, proplists:get_value(<<"status">>, Gr))
    ].

fullput_ommitted_flag(_)->
    Binary = <<"fullputget_no_flag">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    Pmsgid = unique_id(),
    Pr = generate_publish(
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, Pmsgid},
	     {ext, ""}
	    ], [{octets, Binary}]),
    timer:sleep(50),

    Gmsgid = unique_id(),    
    Gr = generate_get(Name, Gmsgid, "ext", ?GETURL),    
    [
     ?_assertEqual(
	list_to_binary(Pmsgid), proplists:get_value(<<"msgid">>, Pr)),
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr)),

     ?_assertEqual(
	list_to_binary(Gmsgid), proplists:get_value(<<"msgid">>, Gr)),
     ?_assertEqual(203, proplists:get_value(<<"status">>, Gr))
    ].

publish_meta(_)->
    Name = "ni:///sha-256;pkasNIdi84",
    Pr = generate_publish(
	    [
	     {'URI', Name},
	     {msgid, unique_id()},
	     {ext, "{\"meta\" : {\"key1\":\"object1\",\"key2\":\"object2\" }}"}
	    ], []),
    Gr = generate_get(Name, unique_id(), "ext", ?GETURL),
    {Metadata} =  proplists:get_value(<<"metadata">>, Gr),
    {Meta} = proplists:get_value(<<"meta">>,Metadata),
    [
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),

     ?_assertEqual(<<"object1">>, proplists:get_value(<<"key1">>, Meta)),
     ?_assertEqual(<<"object2">>, proplists:get_value(<<"key2">>, Meta))
    ].

metadata_merge(_)->
    Name = "ni:///sha-256;ojasNIdi73",
    Key1 = "metakey1", Obj1 = "meta_object1",
    Key2 = "metakey2", Obj2 = "meta_object2",
    Key3 = "metakey3", Obj3 = "meta_object3",
    Pr1 = generate_publish(
	    [
	     {'URI', Name }, 
	     {msgid, unique_id()}, 
	     {ext, "{\"meta\": { \""++
		  Key1 ++ "\" : \"" ++ Obj1 ++ "\", \""++
		  Key2 ++ "\":\"" ++ Obj2 ++ "\"}}"}
	    ], []),
    generate_publish(
    	    [
    	     {'URI', Name }, 
    	     {msgid, unique_id()}, 
    	     {ext, "{\"meta\": { \""++ Key3 ++ "\" : \"" ++ Obj3 ++ "\" }}"}
    	    ], []),

    GetR1 = generate_get(Name, unique_id(), "ext", ?GETURL),
    {GetR1_metadata} = proplists:get_value(<<"metadata">>, GetR1),
    {Meta} = proplists:get_value(<<"meta">>,GetR1_metadata),
    [
     ?_assertEqual(203, proplists:get_value(<<"status">>, GetR1)),
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr1)),
     ?_assertNotEqual(undefined, proplists:get_value(<<"msgid">>, Pr1)),
     
     ?_assertEqual(list_to_binary(Obj1), 
		   proplists:get_value(list_to_binary(Key1), Meta)),
     ?_assertEqual(list_to_binary(Obj2), 
		   proplists:get_value(list_to_binary(Key2), Meta)),
     ?_assertEqual(list_to_binary(Obj3), 
		   proplists:get_value(list_to_binary(Key3), Meta))
    ].

fullput_collision(_)->
    Binary = <<"fullput_collision">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    generate_publish(
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, unique_id()}, 
	     {ext, ""},
	     {fullPut, "true"}
	    ], [{octets, Binary}]),
    timer:sleep(10),
    Pr = generate_publish(
	    [
	     {'URI', Name ++ "?ct=" ++ Ct}, 
	     {msgid, unique_id()}, 
	     {ext, ""},
	     {fullPut, "true"}
	    ], [{octets, Binary}]),
    Gr = generate_get(Name, unique_id(), "ext", ?GETURL),    
    [   
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Pr)),

     ?_assertEqual(200, proplists:get_value(<<"status">>, Gr)),
     ?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Gr)),
     ?_assertEqual(list_to_binary(Ct), proplists:get_value(<<"ct">>, Gr)),
     ?_assertEqual(Binary, proplists:get_value(<<"octets">>, Gr))
    ].


fullput_collision_bogus_hash(_)->
    Binary = <<"fullput_collision_bogus_hash">>,
    Ct = "text/plain",
    Hash = strip(base64:encode(crypto:hash(sha256, Binary))),
    Name = "ni:///sha-256;" ++ Hash,
    unique_id(),
    generate_publish(
   	    [
   	     {'URI', Name ++ "?ct=" ++ Ct}, 
   	     {msgid, unique_id()}, 
   	     {ext, ""},
   	     {fullPut, "true"}
   	    ], [{octets, Binary}]),
    timer:sleep(10),
    Pr1 = generate_publish(
    	    [
    	     {'URI', Name ++ "?ct=" ++ Ct}, 
    	     {msgid, unique_id()}, 
    	     {ext, ""},
    	     {fullPut, "true"}
    	    ], [{octets, <<"data not matching the hash">>}]),
    Gr = generate_get(Name, unique_id(), "ext", ?GETURL),
    [   
   	?_assertEqual(406, proplists:get_value(<<"status">>, Pr1)),

   	?_assertEqual(200, proplists:get_value(<<"status">>, Gr)),
   	?_assertEqual(list_to_binary(Name), proplists:get_value(<<"ni">>, Gr)),
   	?_assertEqual(list_to_binary(Ct), proplists:get_value(<<"ct">>, Gr)),
   	?_assertEqual(Binary, proplists:get_value(<<"octets">>, Gr))
    ].

test_hash_truncation(_)->
    Binary = <<"fullput-sha256-64">>,
    Hash = strip(
	     base64:encode(
	       truncate_hash(
		 crypto:hash(sha256, Binary), 64)
	      )),
    HashNoTrunc =  strip(
	     base64:encode(
		 crypto:hash(sha256, Binary)
		    )),
    Name = "ni:///sha-256-64;" ++ Hash,
    Name2 = "ni:///sha-256-64;"++ HashNoTrunc,
     Pr =  generate_publish(
	    [
	     {'URI', Name}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {fullPut, "true"},
	     {loc1, ?GENERIC_LOCATOR "/loc1"},
	     {loc2, ?GENERIC_LOCATOR "/loc2"}
	    ], [{octets, Binary}]),
    Pr2 = generate_publish(
	    [
	     {'URI', Name2}, 
	     {msgid, unique_id()}, 
	     {ext, "{\"ig\": \"apa\"}"}, 
	     {fullPut, "true"},
	     {loc1, ?GENERIC_LOCATOR "/loc1"},
	     {loc2, ?GENERIC_LOCATOR "/loc2"}
	    ], [{octets, Binary}]),
    [
     ?_assertEqual(201, proplists:get_value(<<"status">>, Pr)),
     ?_assertEqual(406, proplists:get_value(<<"status">>, Pr2))
    ].

test_path(_) ->
    Wkn = "http://localhost:9999/.well-known/netinfproto/get",
    Bogus = "http://localhost:9999/etinfproto/get",
    WGR = generate_get(
	    "ni:///sha-256;niASDFgIdj2whatever", unique_id(), "ext", Wkn),
    BGR = generate_get(
	    "ni:///sha-256;niASDFgIdj2", unique_id(), "ext", Bogus),
    [
     ?_assertEqual(404, proplists:get_value(<<"status">>, WGR)),
     ?_assertEqual(400, proplists:get_value(<<"status">>, BGR))
    ].

test_content_transfer(_) ->
    Id = unique_id(),
    Binary = list_to_binary(Id),
    Hash = hash(Binary),
    Name = "ni:///sha-256-64;" ++ Hash,
    
    generate_publish(
      [
       {'URI', Name}, 
       {msgid, unique_id()}, 
       {ext, "{\"ig\": \"apa\"}"}, 
       {fullPut, "true"},
       {loc1, "loc1"},
       {loc2, "loc2"}
      ], [{octets, Binary}]),
    FetchURL = "http://localhost:8078/octets/" 
	++ binary_to_list(cowboy_http:urlencode(list_to_binary(Name))),
    BadFetch = FetchURL ++ "crap",
    BadURL = "http://localhost:8078/octts/"
	++ binary_to_list(cowboy_http:urlencode(list_to_binary(Name))),
    Reply = httpc:request(FetchURL),
    BadReply = httpc:request(BadFetch), 
    BadURLRep = httpc:request(BadURL),

    [
     ?_assertMatch({ok, {{_, 200, _}, 
			 _, 
			 Id}}, Reply),
    ?_assertMatch({ok, {{_, 404, _}, 
			 _, 
			 []}}, BadReply),
    ?_assertMatch({ok, {{_, 404, _}, 
			 _, 
			 []}}, BadURLRep)
    ].


test_content_put(_) ->
    Id = unique_id(),
    Binary = list_to_binary(Id),
    Hash = hash(Binary),
    Name = "ni:///sha-256-64;" ++ Hash,
    
    ReplyPost = generate_post(
		  [
		   {chunkName, Name}
		  ], 
		  [{octets, Binary}],
		 "http://localhost:8079/cache"
		 ),
    timer:sleep(1000),
    FetchURL = "http://localhost:8078/octets/" 
	++ binary_to_list(cowboy_http:urlencode(list_to_binary(Name))),
    Reply = httpc:request(FetchURL),
    
    [
     ?_assertMatch({ok, {{_, 200, _}, _, _}}, ReplyPost),
     ?_assertMatch({ok, {{_, 200, _}, _, Id}}, Reply)
    ].

test_multiple_requests() ->
    {_, Start, _} = now(),
    N = 100,
    ByteSize = 100,
    ?debugMsg(
       "Will start " 
       ++ integer_to_list(N) 
       ++ " search-(full)publish-search-get with octet size: "
       ++ integer_to_list(ByteSize)
      ),
    Sp = n_s_pub_s_get(100),
    {_, Stop, _} = now(),
    ?debugMsg(
       integer_to_list(N) 
       ++" search-publish-search-get took " 
       ++ integer_to_list(Stop - Start) 
       ++ " seconds."
      ),
    [ 
      ?_assert(lists:all(fun(S) -> S =:= 200 end, Sp))
    ].

    
    	
%%%====================================================================
%%% Helper functions
%%%====================================================================

strip(Hash) ->
    string:strip(binary_to_list(Hash), both, $=).

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
	    HList ++ nn_util:decode(ResponseBody)
    end.

generate_post(PropList, Data, URL) ->
    Boundary = "------WebKitFormBoundaryjTwy4nYi2Aj6shCW",
    Body = format_multipart_formdata(
	     Boundary, PropList, Data),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(byte_size(Body))}],
    
    httpc:request(post, 
		  {URL, Headers, ContentType, Body}, 
		  [], 
		  [{body_format, binary}]).

generate_search(MsgID, Tokens, Ext)->
   {ok, {_, HList, Body}} = 
	httpc:request(post, 
		      {?SEARCHURL, 
		       [], 
		       "application/x-www-form-urlencoded",
		       "msgid="++ MsgID ++ "&tokens=" ++ Tokens ++ "&ext=" ++ Ext
		      }, [], []),
    HList ++ nn_util:decode(Body).

generate_publish(Publish, Data) ->
    Boundary = "------WebKitFormBoundaryjTwy4nYi2Aj6shCW",
    Body = format_multipart_formdata(
	     Boundary, Publish, Data),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(byte_size(Body))}],
    
    case httpc:request(post, {?PUBLISHURL, Headers, ContentType, Body}, 
		       [], [{body_format, binary}]) of
	{ok, {_, [{_, <<"multipart/form-data;", Rest/binary>>}], ResponseBody}} ->
	    nn_util:decode(Rest, ResponseBody);
	{ok, {_, HList, ResponseBody}} ->
	    HList ++ nn_util:decode(ResponseBody)
    end.

format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(
		   fun({FieldName, FieldContent}) ->
			  format_one_part(FieldName, FieldContent, Boundary)
		   end, Fields),
    FieldParts2 = list_to_binary(FieldParts),

    FileParts = lists:map(
		  fun({FieldName, FileContent}) ->
			  format_one_part(FieldName, FileContent, Boundary)
		  end, Files),
    FileParts2 = list_to_binary(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    list_to_binary([
		    FieldParts2, FileParts2, EndingParts
		   ]).

format_one_part(octets, Binary, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(octets), "\""]),
     lists:concat([
		   "Content-Type: ", "application/octet-stream"]),
     "\r\n\r\n",
     Binary, "\r\n"];
format_one_part(FieldName, FieldContent, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(FieldName), "\""
		  ]),
     "\r\n\r\n",
     FieldContent, "\r\n"].

do_n_pubs(N) ->
    do_n_pubs(0, N, []).

do_n_pubs(N, N, Acc) ->
    Acc;
do_n_pubs(N, M, Acc) ->
    Name = "ni:///sha-256;niasNIdia2"
	++ unique_id()
	++ "?ct=text/plain",
    Pr = generate_publish(
	   [
	    {'URI', Name}, 
	    {msgid, unique_id()}, 
	    {ext, "{\"ig\": \"apa\"}"}, 
	    {loc1, "loc1"},
	    {loc2, "loc2"}
	   ], []), 
    do_n_pubs(N + 1, M, [Pr | Acc]).

n_s_pub_s_get(N) ->
    n_s_pub_s_get(N, 0, []).

n_s_pub_s_get(N, N, Acc) ->
    search_and_get(Acc) ;
n_s_pub_s_get(N, M, Acc) ->
    Binary = nn_util:n_byte_binary(100),
    Token = 
	"jagalskarhokkyhokkyhokkyoomduintegordetsaarduriktigtpuckad"
	++unique_id(),
    Name = "ni:///sha-256-64;" ++ hash(Binary),
    generate_search(unique_id(), Token, []),
    generate_publish([ 
		       {'URI', Name}, 
		       {msgid, unique_id()},
		       {loc, ?GENERIC_LOCATOR},
		       {fullPut, "true"},
		       {ext, 
			"{\"meta\": {\"" ++ "token1" ++ "\" : \"" ++ Token ++  "\" }}" 
		       }		       
		     ], [{octets, Binary}]),    
    n_s_pub_s_get(N, M+1, [Token | Acc]).
    
search_and_get(Tokens) ->
    search_and_get(Tokens, []).

search_and_get([], Acc) ->
    Acc;
search_and_get([T | Ts], Acc) ->
    Sr = generate_search(unique_id(), T, []),
    
    [{Res}] = proplists:get_value(<<"results">>, Sr),
    Ni = proplists:get_value(<<"ni">>, Res),
    Gr = generate_get(binary_to_list(Ni), unique_id(), "", ?GETURL),
    timer:sleep(100),
    search_and_get(Ts, [proplists:get_value(<<"status">>, Gr) | Acc]).

unique_id() ->
    {_, _, Micros} = now(),
    integer_to_list(Micros).
 
truncate_hash(HashedBinary, TruncationNumber)->
    <<TruncHash:TruncationNumber, _/binary >> = HashedBinary,
    <<TruncHash:TruncationNumber>>.

hash(Binary) ->
    strip(
      base64:encode(
	truncate_hash(
	  crypto:hash(sha256, Binary), 64)
       )).


