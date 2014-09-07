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
%%% @author Thomas Nordström
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2012 by Marcus Ihlar <marcus@marcus>

-module(nn_msgid_store_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log,
		fun(_, _, _, Msg) -> Msg end),
    meck:new(nn_stats),
    meck:expect(nn_stats, update,
		fun(_, _) -> ok end),
    meck:expect(nn_stats, add_to_list,
		fun(_, _) -> ok end),
    meck:expect(nn_stats, remove_from_list,
		fun(_, _) -> ok end),
    nn_msgids:init(),
    {ok, Pid} = nn_msgid_sup:start_link(),
    Pid.

teardown(Sup) ->
    meck:unload(nn_logger_server),
    meck:unload(nn_stats),
    exit(Sup, shutdown).

storage_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(Sup) ->
	     [
	      test_insert(Sup),
	      test_lookup(Sup),
	      test_timeout(Sup),
	      test_delete(Sup)
	     ] 
     end
    }.

test_insert(Pid) ->
    Res1 = nn_msgid_store:insert(123, Pid),
    Res2 = nn_msgid_store:insert(123, Pid),
    
    [
     ?_assertEqual(ok, Res1),
     ?_assertMatch({error, _}, Res2)
    ].

test_lookup(Pid) ->
    Res1 = nn_msgid_store:lookup(bogus),
    nn_msgid_store:insert(bogus, Pid),
    Res2 = nn_msgid_store:lookup(bogus),
    [
     ?_assertEqual({ok, no_match}, Res1),
     ?_assertEqual({ok, Pid}, Res2)
    ].

test_timeout(Pid) ->
    nn_msgid_store:insert(timeout, Pid, 2),
    Res1 = nn_msgid_store:lookup(timeout),
    timer:sleep(2500),
    Res2 = nn_msgid_store:lookup(timeout),
    [
     ?_assertEqual({ok, Pid}, Res1),
     ?_assertEqual({ok, no_match}, Res2)
    ].

test_delete(Pid) ->
    nn_msgid_store:insert(delete, Pid),
    Res1 = nn_msgid_store:lookup(delete),
    Res2 = nn_msgid_store:delete(delete),
    Res3 = nn_msgid_store:lookup(delete),
    [
     ?_assertEqual({ok, Pid}, Res1),
     ?_assertEqual(ok, Res2),
     ?_assertEqual({ok, no_match}, Res3)
    ].
