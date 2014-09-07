%%%-------------------------------------------------------------------
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

%%% @author Jon Borglund
%%% @author Daniele Bacarella
%%% @doc
%%% This module contains tests for the nn_database_riak module
%%% @end
%%% Created : 14 Nov 2012 by Daniele
%%%-------------------------------------------------------------------

 -module(nn_database_riak_tests).

-include_lib("eunit/include/eunit.hrl").
-define(BUCKET, <<"netinf_bucket">>).

 set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_, _, _, _) -> ok end),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test1">> ),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test2">> ),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test3">> ).

clear_test_state(_) ->
    meck:unload(nn_logger_server),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test1">> ),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test2">> ),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"test3">> ).
    
db_riak_test_() ->
    {foreach,
     fun set_test_state/0,
     fun clear_test_state/1,
      [
       fun pub/1,
      fun get/1,
      fun search/1,
      fun unpublish/1
% add test connection to db
     ]}.

pub(_) ->  
    NewProto = nn_proto:new(<<"test1">>),
    [?_assertEqual({ok, NewProto}, nn_database_riak:publish(NewProto))].
    

get(_) ->
    NewProto = nn_proto:new(<<"test1">>),
    [?_assertEqual({ok, NewProto}, nn_database_riak:publish(NewProto)),
     ?_assertEqual({ok, NewProto}, nn_database_riak:get(<<"test1">>)),
     ?_assertEqual({ok, no_match}, 
		   nn_database_riak:get("none existing object name"))
    ].
    
  
search(_) ->
    {ok, InputJson1} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"puppy\"]}}">>),
    {ok, InputJson2} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cute puppy\"]}}">>),
    Proto1 = 
	nn_proto:new(<<"test1">>, 
		     [<<"locator 2">>, <<"locator 1">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    Proto2 = 
	nn_proto:new(<<"test2">>, 
		     [<<"locator 2">>, <<"locator 1">>],
		     InputJson2,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),

   % Fix data type for the field 'name' in nn_proto:proto()
   % binary_to_term(<<"name">>) gets BADARGH error
    nn_database_riak:publish(Proto1),
    nn_database_riak:publish(Proto2),

    %ProtoList = lists:sort([Proto1, Proto2]),
    %Result1 = nn_database_riak:search([<<"1">>], ProtoList),
    %Result2 = nn_database_riak:search([<<"cute">>], ProtoList),
    %Result3 = nn_database_riak:search([<<"cat">>], ProtoList),
    %Result4 = nn_database_riak:search([<<"dog">>], ProtoList),
    {ok, ListResults} = nn_database_riak:search([<<"puppy">>]),
     {ok, ListResults2} = nn_database_riak:search([<<"trjefefl">>]),
   
    [
     ?_assertEqual(2, length(ListResults)),
     ?_assertEqual(no_match, ListResults2)
     %?_assertEqual([Proto2], Result2),
     %?_assertEqual(ProtoList, Result3),
     %?_assertEqual([], Result4)
    ].


unpublish(_) ->
    
    {ok, InputJson1} = 
	json:decode(<<"{\"ct\": \"image/png\",\"meta\":{\"tag\":[1, 2, \"ggygy\"]}}">>),
  
    Proto1 = 
	nn_proto:new(<<"test1">>, 
		     [<<"A">>, <<"B">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    Proto2 = 
	nn_proto:new(<<"test1">>, 
		     [<<"B">>, <<"C">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),

    Proto3 = 
	nn_proto:new(<<"test1">>, 
		     [<<"A">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    nn_database_riak:publish(Proto1),
    {ok, ProtoUnpub1} = nn_database_riak:unpublish(Proto2),
    nn_database_riak:unpublish(Proto3),
    Name = nn_proto:get(name, Proto1),
    [
     ?_assertEqual({ok, no_match} , nn_database_riak:get(Name)),
     ?_assertEqual([<<"A">>], nn_proto:get(uri, ProtoUnpub1))
    ].

    
%%%===================================================================
%%% Internal functions
%%%===================================================================

pb_link() ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    Pid.

    
