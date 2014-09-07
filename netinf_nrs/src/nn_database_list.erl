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
%%% @author Alex Lindholm 
%%% @author Kiril Goguev
%%% @author Faroogh Hassan
%%% @doc
%%%  This module publishes, retrieves, removes and searches for NDO's
%%%  in the database. 
%%% @end
%%% Created : 7 Nov 2012 by Kiril Goguev
%%%-------------------------------------------------------------------
-module(nn_database_list).

-include_lib("eunit/include/eunit.hrl").

-behaviour(nn_database).
-behaviour(gen_server).

%% nn_database callbacks
-export([init/0, 
	 publish/1, 
	 get/1, 
	 unpublish/1, 
	 search/1,
	 flush/0
	]).

%% API
-export([start_link/0,
	 start/0,
	 stop/0,
	 store_list/1
	]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         terminate/2, 
	 code_change/3
	]).

%% Test helpers
-export([set_test_state/0, 
	 clear_test_state/1
	]).

-define(SERVER, ?MODULE).

-record(state, {
	  data = [],
	  lease_time = 0 :: integer(),
	  start_time = 0 :: integer()
	 }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% connect to the <i>database</i>. 
%% @end
%%--------------------------------------------------------------------

-spec init()
	     ->{ok, atom()}|{error, string()}.
init()->
    start_link(),
    {ok, ?SERVER}.


%%--------------------------------------------------------------------
%% @doc 
%% store/merge the <i>NetInfObject</i> in the storage. 
%% @end
%%--------------------------------------------------------------------
-spec publish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()} | {ok, no_match}.
publish(NetInfObject) ->
    gen_server:call(?SERVER, {publish, NetInfObject}).

%%--------------------------------------------------------------------
%% @doc returns the NetInfObject with the name <i>Name</i> if present
%% in the storage.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: string()) 
	 -> {ok, nn_proto:proto()} | {ok, no_match}.
get(Name) ->
    gen_server:call(?SERVER, {get, Name}).

%%--------------------------------------------------------------------
%% @doc removes the locators present in <i>NetInfObject</i> from the stored
%% NetInfObject, if it was the last occurance of locators the whole
%% object is removed.
%% @end
%%--------------------------------------------------------------------
-spec unpublish(NetInfObject :: nn_proto:proto()) 
	 -> {ok, ReturnNetInfObj :: nn_proto:proto()} | {ok, no_match}.
unpublish(NetInfObject) ->
    gen_server:call(?SERVER, {unpublish, NetInfObject}).

%%--------------------------------------------------------------------
%% @doc search the <i>database</i> for the NetInfObjects which match the
%% search keywords in the search list. 
%%--------------------------------------------------------------------
-spec search(SearchList :: list())
	     ->{ok, list()}|{ok, no_match}.
search(SearchList)->
    gen_server:call(?SERVER,{search,SearchList}).

%%--------------------------------------------------------------------
%% @doc
%% flush all the data. 
%% @end
%%--------------------------------------------------------------------
-spec flush()
	     -> ok.
flush() ->
    gen_server:call(?SERVER, flush).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok, TimerListDB}=application:get_env(netinf_nrs, list_timer),
    case file:consult("files/ListDatabase") of
	{ok, [ListDatabase]} ->
	    {ok, 
	     #state{data=ListDatabase,
		    lease_time = TimerListDB,
		    start_time = StartTime}, 
	    nn_util:time_left(StartTime, TimerListDB)};
	_ ->
	    
	    {ok,
	     #state{data = [], 
		    lease_time = TimerListDB, 
		    start_time = StartTime},
	     nn_util:time_left(StartTime, TimerListDB) }	    
    end.
	    

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({publish, NetInfObject}, _, State) ->
    nn_logger_server:log(verbose, 
			 ?MODULE, handle_call, ["before add_to_list"]),
    {NewObject, NewData} = add_to_list(State#state.data, NetInfObject),
    nn_logger_server:log(verbose, ?MODULE, handle_call,
			 ["Publishing ", NewObject]),
    NewState = State#state{data = NewData},
    StartTime = State#state.start_time,
    LeaseTime = State#state.lease_time,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    {reply, {ok, NewObject}, NewState, TimeLeft};

handle_call({get, Name}, _, State) ->
    nn_logger_server:log(verbose, ?MODULE, handle_call,
			 ["Getting: ", Name]),
    Data = get_stored_with_name(Name, State#state.data),
    nn_logger_server:log(verbose, ?MODULE, handle_call,
				 ["Fetching"]),
    StartTime = State#state.start_time,
    LeaseTime = State#state.lease_time,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    {reply, {ok, Data}, State, TimeLeft};

handle_call({unpublish, NetInfObject}, _, State) ->
    {NewObject, NewData} = remove_from_list(State#state.data, NetInfObject),
    StartTime = State#state.start_time,
    LeaseTime = State#state.lease_time,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    {reply, {ok, NewObject}, State#state{data = NewData}, TimeLeft};
handle_call({search, SearchList}, _, State) ->
    Data = State#state.data,
    StartTime = State#state.start_time,
    LeaseTime = State#state.lease_time,
    TimeLeft = nn_util:time_left(StartTime, LeaseTime),
    case search(SearchList, Data) of
	[] ->
	    {reply, {ok, no_match}, State, TimeLeft};
	Reply ->
	    {reply, {ok, lists:usort(Reply)}, State, TimeLeft}
    end;
handle_call(flush, _, State) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok, TimerListDB}=application:get_env(netinf_nrs, list_timer),
    {reply, ok, State#state{
		  data = [],
		 lease_time = TimerListDB,
		 start_time = StartTime}, 
     nn_util:time_left(StartTime, TimerListDB)}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(timeout, State) ->
    store_list(State#state.data),
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok, TimerListDB}=application:get_env(netinf_nrs, list_timer),
    {noreply, State#state{
		start_time = StartTime, 
		lease_time = TimerListDB},  nn_util:time_left(StartTime, TimerListDB)};

handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    store_list(_State#state.data),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
add_to_list(L, NetInfObject) ->
    add_to_list(L, NetInfObject, []).
add_to_list([], NetInfObject, Ack) ->
    {NetInfObject, [NetInfObject|Ack]};
add_to_list([Stored|T], NetInfObject, Ack) ->
    nn_logger_server:log(verbose, ?MODULE, add_to_list, ["add_to_list"]),
    StoredName = nn_proto:get(name, Stored),
    NewName = nn_proto:get(name, NetInfObject),
    case StoredName =:= NewName of
	true ->
	    nn_logger_server:log(verbose, 
				 ?MODULE, add_to_list, ["true case, going to merge ", 
								 StoredName]),
	    MergedNetInfObject = nn_merging:merge(Stored, NetInfObject),
	    nn_logger_server:log(verbose,
				 ?MODULE, add_to_list, ["true case got back from nn_mering:merge"]),
	    {MergedNetInfObject, [MergedNetInfObject|T++Ack]};
	false ->
	    nn_logger_server:log(verbose, ?MODULE, add_to_list, ["false case"]),
	    add_to_list(T, NetInfObject, [Stored|Ack])
    end.

remove_from_list(L,NetInfObject) ->
    remove_from_list(L, NetInfObject, []).
remove_from_list([], _, Ack) ->
    {no_match, Ack};
remove_from_list([Stored|T], NetInfObject, Ack) ->
    StoredName = nn_proto:get(name, Stored),
    NewName = nn_proto:get(name, NetInfObject),
    case StoredName =:= NewName of
	true ->
	    DiffNetInfObject = diff(Stored, NetInfObject),
	    case nn_proto:get(uri, DiffNetInfObject) =:= [] of
		true ->
		    {DiffNetInfObject, T++Ack};
		false ->
		    {DiffNetInfObject, [DiffNetInfObject|T++Ack]}
	    end;
	false ->
	    remove_from_list(T, NetInfObject, [Stored|Ack])
    end.


store_list(ListData)->  
    case ListData of
	[] ->
	    ok;
	_->
	    file:write_file("files/ListDatabase", io_lib:fwrite("~p.\n", [ListData]))
    end.
    
diff(Stored, NetInfObject) ->
    MinLocators = nn_proto:get(uri, Stored),
    SubLocators = nn_proto:get(uri, NetInfObject),
    nn_proto:set([{uri, MinLocators -- SubLocators}], Stored).
    
get_stored_with_name(_, []) ->
    no_match;
get_stored_with_name(Name, [H|T]) ->
    case Name =:= nn_proto:get(name, H) of
	true ->
	    H;
	false ->    
	    get_stored_with_name(Name, T)
    end.

search([],_) ->
    [];
search([Hd|Tl],StoredObjects) ->
    search_objects(Hd,StoredObjects) ++ search(Tl,StoredObjects).

search_objects(_, []) ->
    [];
search_objects(SearchKey, [Hd|Tl]) ->
    case search_object(SearchKey, nn_proto:get(ext,Hd)) of
	match ->
	    [Hd | search_objects(SearchKey, Tl)];
	_ ->
	    search_objects(SearchKey, Tl)
    end.

search_object(_,[]) ->
    nomatch;
search_object(<<"">>, _) ->
    nomatch;
search_object(SearchKey, {JsonObjectList}) ->
    search_object(SearchKey,JsonObjectList);
search_object(SearchKey, [{_,Hd}|Tl]) when is_tuple(Hd) orelse is_list(Hd)->
    case search_object(SearchKey,Hd) of
	match ->
	    match;
	_ ->
	    search_object(SearchKey,Tl)
    end;
search_object(SearchKey, [{_,Hd}|Tl]) ->
    search_object(SearchKey,[Hd|Tl]);
search_object(SearchKey, [Hd|Tl]) when Hd =:= <<"">> ->
    search_object(SearchKey,Tl);	     
search_object(SearchKey, [Hd|Tl]) when is_binary(Hd) ->
    case Hd =:= SearchKey of
	false ->
	    search_object(SearchKey,Tl);
	_ ->
	    match
    end;
search_object(SearchKey, [Hd|Tl]) ->
    case safe_list_to_integer(binary_to_list(SearchKey)) of
	{ok, not_integer} ->
	    search_object(SearchKey, Tl);
	{ok, Integer} ->
	    case Integer == Hd of
		true ->
		    match;
		_ ->
		    search_object(SearchKey, Tl)
	    end
    end.

safe_list_to_integer(L) ->
    safe_list_to_integer(L, 0).

safe_list_to_integer([], Ack) ->
    {ok, Ack};
safe_list_to_integer([Hd|Tl], Ack) when Hd >= $0 andalso Hd =< $9 ->
    safe_list_to_integer(Tl,Ack*10 + Hd - $0);
safe_list_to_integer(_,_) ->
    {ok, not_integer}.

%%%=====================================================================
%%% Testing API
%%%=====================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_,_,_,_) -> ok end).

clear_test_state(_) ->
    meck:unload(nn_logger_server).

run_test_() ->
    {foreach,
     fun set_test_state/0,
     fun clear_test_state/1,
     [
      fun test_search/1
     ]}.

test_search(_) ->
    {ok, InputJson1} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cat\"]}}">>),
    {ok, InputJson2} = 
	json:decode(<<"{\"ct\": \"image/png\",
           \"meta\":{\"tag\":[1, 2, \"cute cat\"]}}">>),
    Proto1 = 
	nn_proto:new(<<"name">>, 
		     [<<"locator 2">>, <<"locator 1">>],
		     InputJson1,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    Proto2 = 
	nn_proto:new(<<"name">>, 
		     [<<"locator 2">>, <<"locator 1">>],
		     InputJson2,
		     undefined %"2012-12-12T12:12:12+00:00"
		    ),
    ProtoList = lists:sort([Proto1,Proto2]),
    Result1 = search([<<"1">>], ProtoList),
    Result2 = search([<<"cute">>], ProtoList),
    Result3 = search([<<"cat">>], ProtoList),
    Result4 = search([<<"dog">>], ProtoList),
    [
     ?_assertEqual(ProtoList,Result1),
     ?_assertEqual([Proto2], Result2),
     ?_assertEqual(ProtoList, Result3),
     ?_assertEqual([], Result4)
    ].
