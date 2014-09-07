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
%%% @author Marcus Ihlar
%%% @doc
%%%  Tests for netinf_proto
%%% @end
%%% Created : 18 Oct 2012 by Marcus Ihlar <marcus@marcus>
%%%-------------------------------------------------------------------
-module(nn_proto_tests).

-include_lib("eunit/include/eunit.hrl").

set_test_state() ->
    ok.

proto_test_() ->
    {foreach,
     fun set_test_state/0,
     [
      fun set/1,
      fun get/1,
      fun new/1
     ]}.

set(_) ->
    OldProto = nn_proto:new("test"),
    NewProto = nn_proto:set([
				     {name, "new name"}, {time_stamp, "coding time"},
				     {uri, ["nice locator"]},{ext, ["metadata"]}
				    ], OldProto),
    NewName = nn_proto:get(name, NewProto),
    NewUri = nn_proto:get(uri, NewProto),
    NewExt = nn_proto:get(ext, NewProto),
    NewTimeStamp = nn_proto:get(time_stamp, NewProto),
    [
     ?_assertEqual(NewName, "new name"),
     ?_assertEqual(NewUri, ["nice locator"]),
     ?_assertEqual(NewExt, ["metadata"]),
     ?_assertEqual(NewTimeStamp, "coding time")
    ].

get(_) ->
    OldProto = nn_proto:new("test"),
    Proto = nn_proto:set([
				  {name, "new name"}, {time_stamp, "coding time"}, 
				  {uri, ["nice locator"]}, {ext, ["metadata"]}
				 ], OldProto),
    [
     ?_assertEqual(nn_proto:get(name, Proto), 
		   nn_proto:get(name, Proto)),
     ?_assertEqual(nn_proto:get(uri, Proto), 
		   nn_proto:get(uri, Proto)),
     ?_assertEqual(nn_proto:get(ext, Proto), 
		   nn_proto:get(ext, Proto)),
     ?_assertEqual(nn_proto:get(time_stamp, Proto), 
		   nn_proto:get(time_stamp, Proto))
    ].

new(_)->
    Proto = nn_proto:new("newRecord"),
    Proto2 = nn_proto:new(
	     "secondNewRecord", ["uri-loc"], ["meta-data"], "testing time"),
    [
     ?_assertEqual(Proto, {netinf, "newRecord", [], {[]}, 
			   undefined}),
     ?_assertEqual(Proto2, {netinf, "secondNewRecord", ["uri-loc"],
			    ["meta-data"], "testing time"})
    ].

