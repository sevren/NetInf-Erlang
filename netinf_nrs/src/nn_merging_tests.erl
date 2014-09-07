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
%%% @author Alex Lindholm
%%% @doc
%%%  Unit tests for the merging API functions.
%%% @end
%%% Created : 5 Nov 2012 by Kiril Goguev
%%%-------------------------------------------------------------------
-module(nn_merging_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_,_,_,_) -> ok end).

teardown(_) ->
    meck:unload(nn_logger_server).

merge_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun merge/1
     ]}.

merge(_)->
    OldExt={[{<<"tag">>,
       {[{<<"id">>, {[{<<"a">>, 1}, {<<"b">>, 2}]}}]}}]},
    NewExt={[{<<"ext">>,
       {[{<<"id">>, {[{<<"a">>, 1}, {<<"b">>, 3},{<<"c">>, 5}]}}]}}]},
    NewExt2={[{<<"tag">>, {[{<<"id">>, {[{<<"a">>, 1}, {<<"b">>, 3},
       {<<"c">>,5}, {<<"test">>,
       {[{<<"q">>,45}, {<<"x">>, 4},{<<"y">>, 7},{<<"z">>, 0}]}}]}}]}}]},

    PreNDO = nn_proto:new("ni://sha-256;test"),
    NDO1 = nn_proto:set([{ext, OldExt}],PreNDO),
    NDO2 = nn_proto:set([{ext, NewExt}],PreNDO),
    NDO3 = nn_proto:set([{ext, NewExt2}], PreNDO),

    PreOutput1=nn_merging:merge(NDO2, NDO1),
    PreOutput2=nn_merging:merge(NDO2, NDO3),
    Output1=nn_proto:get(ext,PreOutput1),
    Output2=nn_proto:get(ext,PreOutput2),

    [
     ?_assertEqual({[{<<"tag">>,
                   {[{<<"id">>, {[{<<"a">>, 1}, {<<"b">>, 2}]}}]}}, 
                   {<<"ext">>,
		   {[{<<"id">>, {[{<<"a">>, 1}, {<<"b">>, 3}, {<<"c">>, 5}]}}]}}]}, 
		  Output1),
     ?_assertEqual({[{<<"tag">>, {[{<<"id">>,{[{<<"a">>,1},{<<"b">>, 3},
		   {<<"c">>, 5}, {<<"test">>,
		   {[{<<"q">>, 45},{<<"x">>, 4},{<<"y">>, 7},{<<"z">>, 0}]}}]}}]}},
		   {<<"ext">>, {[{<<"id">>,
				  {[{<<"a">>, 1},{<<"b">>, 3},{<<"c">>,5}]}}]}}]},
		  Output2)
  ].
