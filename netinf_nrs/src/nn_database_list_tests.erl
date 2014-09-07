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
%%% @author Jon Borglund
%%% @author Alexander Lindholm
%%% @author Kiril Goguev
%%% @doc
%%% This module contains tests for the nn_database_list module
%%% @end
%%% Created : 8 Nov 2012 by JON <jon@jon>
%%%-------------------------------------------------------------------

 -module(nn_database_list_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    nn_database_list:set_test_state(),
    nn_database_list:start().
								
teardown(_) ->
    nn_database_list:clear_test_state(ok),
    nn_database_list:stop().
    
    
event_handler_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun pub/1,
      fun get/1,
      fun unpub/1
     ]}.

pub(_) ->
    L1=[{<<"tag">>, {[{<<"a">>,1}]}}],

    NewProto = nn_proto:new("publish"),
    SecondProto = nn_proto:set([{ext, L1}, {uri, L1}], NewProto),

    [?_assertNotEqual(SecondProto, NewProto)].
    



get(_) ->
    NewProto = nn_proto:new("get"),
    [?_assertEqual({ok, NewProto}, nn_database_list:publish(NewProto)),
     ?_assertEqual({ok, NewProto}, nn_database_list:get("get")),
     ?_assertEqual({ok, no_match}, 
		   nn_database_list:get("none existing object name"))
    ].
    

unpub(_) ->
    L1 = ["Loc 1"],
    L2 = ["Loc 2", "Loc 3"],
    L3 = L1 ++ L2,
    NewProto = nn_proto:new("unpublish"),
    SecondProto = nn_proto:set([{ext, L3}, {uri, L3}], NewProto),
    ThirdProto = nn_proto:set([{ext, L2},{uri, L2}], NewProto),
    {ok, MergedProto} = nn_database_list:unpublish(ThirdProto),
    NonExistingProto = nn_proto:new("nonext"),
    [?_assertEqual(L3, L1 ++ L2),
     ?_assertEqual({ok, NewProto}, nn_database_list:publish(NewProto)),
     ?_assertEqual({ok, NewProto}, nn_database_list:get("unpublish")),
     ?_assertEqual({ok, NewProto}, nn_database_list:unpublish(NewProto)),
     ?_assertEqual({ok, no_match}, nn_database_list:get("unpublish")),    
     ?_assertEqual({ok, SecondProto}, nn_database_list:publish(SecondProto)),
     ?_assertEqual(no_match, MergedProto),
     ?_assertEqual({ok, no_match}, 
		   nn_database_list:unpublish(NonExistingProto))
    ].
    
