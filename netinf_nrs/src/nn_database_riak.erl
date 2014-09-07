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
%%% @author Daniele Bacarella
%%% @doc
%%%  This module implements a wrapper for Riak database. 
%%% @end
%%% Created :  Nov 8 2012 by Jon Borglund and Daniele Bacarella
%%%-------------------------------------------------------------------
-module(nn_database_riak).

-behaviour(nn_database).

-include_lib("eunit/include/eunit.hrl").

%% nn_database callbacks
-export([init/0, 
	 publish/1, 
	 get/1, 
	 unpublish/1, 
	 search/1,
	 flush/0
	]).


-define(BUCKET, <<"netinf_bucket">>).


%%--------------------------------------------------------------------
%% @doc 
%% connect to the database 
%% @end
%%--------------------------------------------------------------------

-spec init() -> {ok}.
init()->
    nn_logger_server:log(verbose, ?MODULE, init, ["init Riak database"]),
    {ok}.


%%--------------------------------------------------------------------
%% @doc 
%% Stores/merges the NetInfObject in the database. 
%% @end
%%--------------------------------------------------------------------
-spec publish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()} | {ok, no_match}.
publish(NetInfObject) ->
    Key = nn_proto:get(name, NetInfObject),
    NetInfMergedObject = get_and_merge(NetInfObject),
    Ext = nn_proto:get(ext, NetInfMergedObject),
    FlattenedExt = flatten(Ext),
    MetaDataBinary = join(FlattenedExt),
    RiakObj =  riakc_obj:new(?BUCKET, Key, 
			   [
			    {<<"meta">>, MetaDataBinary},
			    {<<"ndo">>, term_to_binary(NetInfMergedObject)}
			   ], "application/x-erlang"),

    riakc_pb_socket:put(pb_link(), RiakObj),
    nn_logger_server:log(verbose, ?MODULE, publish, ["--- Riak : publishing the NDO into the database~n"]),
    nn_logger_server:log(verbose, ?MODULE, publish, ["NDO published"]),
    {ok, NetInfMergedObject}.

    

%%--------------------------------------------------------------------
%% @doc 
%% returns the NetInfObject with the 'Name' if present
%% in the storage.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: binary()) 
	 -> {ok, nn_proto:proto()} | {ok, no_match}.
get(Name) ->
    case riakc_pb_socket:get(pb_link(), ?BUCKET, Name) of
	{ok, Obj} ->   
	    ValueBin = riakc_obj:get_value(Obj),
	    ValueTerm = binary_to_term(ValueBin),
	    NDO = proplists:get_value(<<"ndo">>, ValueTerm),
	    nn_logger_server:log(verbose, ?MODULE, get, ["--- Riak : getting the NDO from the database~n"]),
	    nn_logger_server:log(verbose, ?MODULE, get, ["NDO : ", [binary_to_term(NDO)]]),
	    {ok, binary_to_term(NDO)};
	_ ->
	    {ok, no_match}
	    
    end.

%%--------------------------------------------------------------------
%% @doc
%% removes the locators present in NetInfObject from the stored
%% NetInfObject, if it was the last occurance of locators the whole
%% object is removed.
%% @end
%%--------------------------------------------------------------------
-spec unpublish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()} | {ok, no_match}.
unpublish(NetInfObject) ->
    KeyBin = nn_proto:get(name, NetInfObject),
    {_, StoredNetInf} = nn_database_riak:get(KeyBin),
    case StoredNetInf of
	no_match ->
	    {ok, no_match};
	_ ->
	    DiffNetInfObject =  diff(StoredNetInf, NetInfObject),
	    nn_logger_server:log(verbose, ?MODULE, unpublish, ["--- Riak : unpublishing the NDO from the database~n"]),
	    case nn_proto:get( uri, DiffNetInfObject) =:= [] of
		true ->
		    riakc_pb_socket:delete( pb_link(), ?BUCKET, KeyBin),
		    {ok, DiffNetInfObject};
		false -> 
		    FlattenedExt = flatten(nn_proto:get(ext, DiffNetInfObject)),
		    MetaDataBinary = join(FlattenedExt),
		    RiakObj =  riakc_obj:new(?BUCKET, KeyBin, 
			   [
			    {<<"meta">>, MetaDataBinary},
			    {<<"ndo">>, term_to_binary(DiffNetInfObject)}
			   ], "application/x-erlang"),
		    riakc_pb_socket:put(pb_link(), RiakObj),
		    {ok , DiffNetInfObject}
	    end    
    end.

   % gen_server:call(?SERVER, {unpublish, NetInfObject}).

%%--------------------------------------------------------------------
%% @doc 
%% searches the database for the NetInfObjects which match the
%% search keywords in the search list. 
%% @end
%%--------------------------------------------------------------------
-spec search(SearchList :: list())
	     ->{ok, list()}|{ok, no_match}.
search(SearchList) ->
    SearchParameter = get_search_param(SearchList),
    nn_logger_server:log(
      verbose, ?MODULE, search, ["--- Riak : searching for NDOs ~n"]),
    nn_logger_server:log(
      verbose, ?MODULE, search, ["Search keys :",  [SearchParameter]]),
    
    case riakc_pb_socket:search(pb_link(), ?BUCKET, SearchParameter) of
	{ok, {search_results, [], _, _}} ->
	    {ok, no_match};
	{ok, {search_results, Results, _, _}} ->
	    {ok, fetch_ndos(Results)};
	_ ->
	    {ok, no_match}
    end.

%%--------------------------------------------------------------------
%% @doc 
%% empties the database 
%% @end
%%--------------------------------------------------------------------	    
-spec flush() -> ok.
flush() ->
    io:format("In flush ~n", []),
    PbLink = pb_link(),
    io:format("In flush ~n", []),
    {ok, Keys} = riakc_pb_socket:list_keys(PbLink, ?BUCKET, 60000),
    flush(Keys, PbLink).

flush([H|T], PbLink) ->
    io:format("~p, ~n", [H]),
    riakc_pb_socket:delete(PbLink, ?BUCKET, H),
    flush(T, PbLink);
flush([], _) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

pb_link() ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    Pid.

%% Flatten tree structure to list
%base case
flatten([]) ->
    [];
%sub object
flatten({P}) when is_list(P) ->
    flatten(P);
%key value list
flatten([{_, V} | T]) ->
    flatten(V) ++ flatten(T);
%list
flatten([V | T]) ->
    flatten(V) ++ flatten(T);
flatten(V) ->
    [V].

join(L) ->
    join(L, []).

join([], A) ->
    list_to_binary(A);
join([H | T], A) when is_binary(H) ->
    Term = binary_to_list(H),
    join(T, Term ++ " " ++ A);
join([H | T], A) when is_number(H) ->
    Term = integer_to_list(H),
    join(T, Term ++ " " ++ A).


get_and_merge(NetInfObject) ->
    KeyBin = nn_proto:get(name, NetInfObject),
   {_, StoredNetInf} = nn_database_riak:get(KeyBin),
    case StoredNetInf of
	no_match ->
	    NetInfObject;
	_ ->
	    nn_merging:merge(NetInfObject, StoredNetInf)
    end.

get_search_param(BinaryList) ->
    StringList = lists:map(fun(Binary) -> binary_to_list(Binary) end, BinaryList),
    KeyWords = string:join(StringList, " OR "),
    list_to_binary("meta:(" ++ KeyWords ++ ")").

    
fetch_ndos(R)->
    R1=proplists:get_all_values(?BUCKET, R),
    fetch_ndos(R1, []).

fetch_ndos([], A) ->
    A;
fetch_ndos([H|T], A) ->
    Ndo = proplists:get_value(<<"ndo">>, H),
    Term = binary_to_term(Ndo),
    %io:format("~n---~n~p~n---~n", [Term]),
    fetch_ndos(T, [Term|A]).
    
diff(Stored, NetInfObject) ->
    MinLocators = nn_proto:get(uri, Stored),
    SubLocators = nn_proto:get(uri, NetInfObject),
    nn_proto:set([{uri, MinLocators -- SubLocators}], Stored).


%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_ , _, _ , _) -> ok end),
    meck:new(nn_merging),
    meck:expect(nn_merging, merge, fun(_, _) -> ok_called end).

clear_test_state(_) ->
    meck:unload(nn_logger_server),
    meck:unload(nn_merging),
    riakc_pb_socket:delete(pb_link(), ?BUCKET, <<"name">>).
 
db_riak_test_() ->
    {foreach,
     fun set_test_state/0,
     fun clear_test_state/1,
     [
      fun test_fetch_ndos/1,
      fun test_get_search_param/1,
      fun test_get_and_merge/1,
      fun test_join/1,
      fun test_flatten/1
     ]}.


test_fetch_ndos(_) ->
    PropList = [{<<"netinf_bucket">>,
		 [{<<"id">>, <<"hash">>},
		  {<<"meta">>, <<"key2">>},
		  {<<"ndo">>,
		   term_to_binary(nn_proto:new(<<"test_ndo">>))
		  }]
		}, 
		{<<"netinf_bucket">>,
		 [{<<"id">>, <<"hash">>},
		  {<<"meta">>, <<"key2">>},
		  {<<"ndo">>,
		   term_to_binary(nn_proto:new(<<"test_ndo 1">>))
		  }]
		}
	       ],
    [
     ?_assertEqual(2, length(fetch_ndos(PropList)))
    ].

test_get_search_param(_) ->
    List = [ <<"wutever">>, << "another" >> ],
    Out = <<"meta:(wutever OR another)">>,
    EmptyOut = <<"meta:()">>,
    [
     ?_assertEqual(Out, get_search_param(List)),
     ?_assertEqual(EmptyOut, get_search_param([])) 
    ].
    

test_get_and_merge(_) ->
    {ok, InputJson1} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cat\"]}}">>),
    {ok, InputJson2} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cute cat\"]}}">>),
    Proto1 = 
	nn_proto:new(<<"name1">>, 
		     [<<"locator 2">>, <<"locator 1">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    Proto2 = 
	nn_proto:new(<<"name">>, 
		     [<<"locator 3">>, <<"locator 1">>],
		     InputJson2,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),

    publish(Proto2),

    [
     ?_assertEqual(Proto1, get_and_merge(Proto1)),
     ?_assertEqual(ok_called, get_and_merge(Proto2))
    ].

test_join(_) ->
    List = [ <<"wutever">>, << "another" >> ],
    Out =  <<"another wutever ">>,
    [
     ?_assertEqual(Out, join(List))
    ].
    
test_flatten(_) ->
	    {ok, InputJson1} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cat\"]}}">>),
    [
     ?_assertEqual([<<"image/png">>, 1, 2, <<"cat">>], flatten(InputJson1) )
    ].
