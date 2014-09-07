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
%%% @author Alexander Lindholm
%%% @author Thomas Nordström
%%% @doc
%%% This module provides functions for merging locators and extensions.
%%% @end
%%% Created : 5 Nov 2012 by Kiril Goguev 
%%%-------------------------------------------------------------------
-module(nn_merging).

-include_lib("eunit/include/eunit.hrl").

%% API Functions
-export([merge/2]).


%%--------------------------------------------------------------------
%% @doc 
%%  Takes two NetInfObjects from nn_proto and merges them
%%  returns a merged nn_proto:proto().
%% @end
%%--------------------------------------------------------------------
-spec merge(New :: nn_proto:proto(), Old :: nn_proto:proto()) ->
			 nn_proto:proto().
merge(New, Old) ->
    OldLocators = lists:usort(nn_proto:get(uri, Old)),
    NewLocators = lists:usort(nn_proto:get(uri, New)),
    MergedLocs = lists:umerge(OldLocators, NewLocators),
    NetinfWithMergedLocs = nn_proto:set([{uri, MergedLocs}], Old),
    {OldExt} = nn_proto:get(ext, Old),

    {NewExt} = nn_proto:get(ext, New),
    nn_logger_server:log(verbose, ?MODULE, merge, ["just before calling merge_lists"]),
    MergedExts = merge_lists(NewExt, OldExt),
    nn_proto:set([{ext, {MergedExts}}], NetinfWithMergedLocs).



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%%  Takes two lists which are supposed to be valid JSON decoded lists from
%%  the JSON library decode function see: deps/json/json.erl
%%  and returns the merged list
%% @end
%%--------------------------------------------------------------------
-spec merge_lists(New :: list(), Old ::list()) ->
			 list().
merge_lists(New, Old) ->
    nn_logger_server:log(verbose, ?MODULE, merge_lists, 
			""),
	  merge_lists(New, Old, []).

merge_lists(New, [], Updated) ->
   New++Updated;


merge_lists([], Old, Updated) ->
    Old++Updated;

merge_lists([{Key, Value}|New_TL], Old, Updated)->
    case proplists:lookup(Key, Old) of
	none->
	    merge_lists(New_TL, Old, [{Key, Value}|Updated]);
	_ ->
	    OldMatch = proplists:get_value(Key, Old),		
	    merge_lists(New_TL,proplists:delete(Key, Old),
	    [merge_objects(Key, OldMatch,Value)|Updated])	   
       end.
	    

merge_objects(Key, Value, Value) ->
    {Key, Value};
merge_objects(Key, OldV, NewV) when is_tuple(OldV) andalso is_tuple(NewV) ->
    OldL = extract_list(OldV),
    NewL = extract_list(NewV),
    {Key, {merge_lists(NewL, OldL)}};
merge_objects(Key, OldV, NewV) when is_list(OldV) andalso is_list(NewV) ->
    {Key, lists:umerge(OldV, NewV)};
merge_objects(Key, OldV, NewV) when is_list(OldV) orelse is_list(NewV) ->
    case is_list(OldV) of
	true ->
	    {Key, lists:umerge(OldV, [NewV])};
	_ ->
	    {Key, lists:umerge([OldV], NewV)}
    end;
merge_objects(Key, OldV, NewV) ->
    {Key, [NewV, OldV]}.

extract_list(Tuple)->
    {Extracted_List}=Tuple, 
    Extracted_List.

%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_,_,_,_) -> ok end).
   

clear_test_state() ->
    meck:unload(nn_logger_server).

%%%=====================================================================
%%% Internal tests
%%%=====================================================================

setup() ->
    set_test_state().

teardown(_) ->
    clear_test_state().

internal_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun merge_l/1,
      fun merge_obj/1,
      fun extl/1
     ]}.
merge_l(_)->
    Test1=[{<<"tag">>, {[{<<"a">>,1}]}}],
    Test2=[],
    Test3=[{<<"tag">>, {[{<<"b">>,2}]}}],
    Test4=[{<<"tag">>, {[{<<"b">>,2}, {<<"a">>,1}]}}],
    Output1=merge_lists(Test1, Test2, []),
    Output2=merge_lists([], Test1, []),
    Output3=merge_lists(Test1, Test3, []),
    [
     ?_assertEqual(Output1, Test1),
     ?_assertEqual(Output1, Output2),
     ?_assertEqual(Test4, Output3)
    ].


merge_obj(_) ->
    Test1=merge_objects(<<"a">>, 1, 1),
    Test2=merge_objects(<<"a">>, {[{<<"b">>, 3}]},{[{<<"c">>, 4}]}),
    Test3=merge_objects(<<"a">>, {[{<<"b">>, 3}]},{[{<<"c">>, 4}]}),
    Test4=merge_objects(<<"a">>, 1, 3),
    [
     ?_assertEqual({<<"a">>, 1}, Test1),
     ?_assertEqual(Test3,Test2),
     ?_assertEqual({<<"a">>, {[{<<"b">>, 3}, {<<"c">>, 4}]} }, Test3),
     ?_assertEqual({<<"a">>, [3, 1]}, Test4)
    ].

extl(_) ->
   Test1=extract_list({[1, 2, 3]}),
    [
     ?_assertNotEqual({1, 2, 3}, Test1),
     ?_assert(is_list(Test1))
    ].
    
