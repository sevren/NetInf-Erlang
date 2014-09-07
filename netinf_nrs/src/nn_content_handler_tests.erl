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
%%% @author Kiril Goguev
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------

-module(nn_content_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    nn_content_handler:set_test_state(),
    nn_content_handler:spawn().

teardown(Pid) ->
    nn_content_handler:kill(Pid),
    nn_content_handler:clear_test_state().
    
event_handler_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun fetch_content/1,
      fun store_content/1
     ]}.

store_content(Pid) ->
    Name = 
	"ni:///sha-256;"++
	"O5w1jzbwoxtq0+FPMJx88ZiskkboMW+c5UPVsZrAK4A", 
    Result = nn_content_handler:store_content(
	       Pid, {Name, <<"file">>}),     
    [
     ?_assertEqual(ok, Result)
    ].

fetch_content(Pid) ->
    Result = nn_content_handler:fetch_content(
	       Pid, "ni:///sha-256;Name"),
    [
     ?_assertEqual({ok, file_not_found}, Result)
    ].
