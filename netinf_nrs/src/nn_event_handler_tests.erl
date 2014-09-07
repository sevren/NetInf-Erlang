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
%%% @author Marcus Ihlar
%%% @doc
%%%  Unit tests for the event handler API functions.
%%% @end
%%% Created : 17 Oct 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(nn_event_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    nn_event_handler:set_test_state(),
    {ok, Pid} = nn_event_handler:spawn(),
    Pid.
								
teardown(Pid) ->
    nn_event_handler:kill(Pid),
    nn_event_handler:clear_test_state().
    
event_handler_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun pub/1,
      fun get/1,
      fun unpub/1
     ]}.

pub(Pid) ->
    Message = nn_proto:new("Name", ["L1", "L2"], ["Meta"], "time"),
    {ok, PubRes} = 
	nn_event_handler:handle_request(Pid, publish, Message),
    Mn = nn_proto:get(name, Message),
    Rn = nn_proto:get(name, PubRes),
    [
     ?_assertNotEqual(PubRes, Message),
     ?_assertEqual(Mn, Rn)
    ].

get(Pid) ->
    Gr = nn_event_handler:handle_request(Pid, get, "Name"),
    [?_assertMatch({ok, _}, Gr)].

unpub(Pid) ->
    Message = nn_proto:new("Name", ["L1", "L2"], ["Meta"], "time"),
    Upr = nn_event_handler:handle_request(Pid, unpublish, Message),
    [
     ?_assertMatch({ok, _}, Upr),
     ?_assertEqual({ok, Message}, Upr)
    ].
    
