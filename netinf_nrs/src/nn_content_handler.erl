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
%%% This module stores and retrieves an NDO. It also handles the full put
%%% where it saves the octects sent along with the pulish request on the 
%%% file system. 
%%% @end
%%% Created :  9 Nov 2012 by Marcus Ihlar <marcus@marcus>
%%%-------------------------------------------------------------------
-module(nn_content_handler).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
	 start_link/0, 
	 validate_and_store_content/2, 
	 fetch_content/2, 
	 spawn/0, 
	 kill/1,
	 store_content_without_validating/3
	]).

%% Test API
-export([set_test_state/0, clear_test_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Spawns a child process for content handler
%% @end
%%--------------------------------------------------------------------
-spec spawn() -> {ok, pid()}.
spawn() ->
    {ok, Pid} = nn_sub_supervisor:start_child(content_handler),
    Pid.

%%--------------------------------------------------------------------
%% @doc
%% Kills the content handler child process
%% @end
%%--------------------------------------------------------------------
-spec kill(pid()) -> ok.
kill(Pid) ->
    gen_server:cast(Pid, kill).
			 
%%--------------------------------------------------------------------
%% @doc
%%  Store binary octets in the file system, this is an asynchronous call.
%%  Takes two pids, the first is the Pid of the gen_server process, the
%%  other is the pid of the caller.
%% @end
%%--------------------------------------------------------------------
-spec validate_and_store_content(Server :: pid(), 
		    Ndo :: {string(), binary()}) -> ok. 
validate_and_store_content(Server, Ndo) ->
    gen_server:call(Server, {store_content, Ndo}).

%%--------------------------------------------------------------------
%% @doc
%%  Retrieve binary octets from 
%%  the file system, this is an asynchronous call.
%%  Takes two pids, the first is the Pid of the gen_server process, the
%%  other is the pid of the caller.
%% @end
%%--------------------------------------------------------------------
-spec fetch_content(pid(), binary()) -> ok.
fetch_content(Server, Name) ->
    gen_server:call(Server, {fetch_content, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Stores content.
%% @end
%%--------------------------------------------------------------------
-spec store_content_without_validating(pid(), binary(), binary()) -> ok.
store_content_without_validating(Server, Name, Data) ->
    gen_server:call(Server, {put_content, Name, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
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
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
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
handle_call({store_content, {Name, Content}}, _, State) ->
    {reply, store({Name, Content}), State};
handle_call({fetch_content, Name}, _, State) ->
    Fetch = fetch(Name),
    {reply, Fetch, State};
handle_call({put_content, Name, Data}, _, State) ->
    {_, Hash} = nn_hash_validation:parse_name(Name),
    Result = put_c(Hash, Data),
    {reply, Result, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(kill, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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

store({Name, Object}) ->
    case nn_hash_validation:validate(Name, Object) of
	{ok, valid, Hash} ->
	    put_c(Hash, Object);
	_ ->
	    {error, invalid_hash}
    end.


put_c(Hash, Object) ->
    FN="files/" ++ binary_to_list(file_name_encode(Hash)),
    case file:read_file_info(FN) of
	{error, enoent} ->
	    nn_stats:update(octet_count, 1),
	    {ok, F} = file:open(FN, [write, binary]),
	    file:write_file(FN, Object),
	    file:close(F);
	_ ->
	    ok
    end,
    ok.

fetch(Name) ->
    {_, Hash} = nn_hash_validation:parse_name(Name), 
    FN="files/" ++ binary_to_list(file_name_encode(Hash)),
    case file:open(FN, [read, binary]) of
	{ok, F} ->
	    nn_logger_server:log(
	      verbose, ?MODULE, 
	      handle_content_transfer, 
	      ["file sent: ", FN]),
	    Content = file:read_file(FN),
	    file:close(F),
	    Content;
	_ ->
	    nn_logger_server:log(
	      verbose, ?MODULE, 
	      handle_content_transfer, 
	      ["file not found : ", FN]),
	    {ok, file_not_found}
    end.
		
file_name_encode(Name) ->
    nn_util:bin_replace(Name, $/, $#).

%%%===================================================================
%%% Testing API
%%%===================================================================

%% sets up all mocks needed for running test in isolation.
%% note that no mocking is done on the nn_proto.
set_test_state() ->
    meck:new(nn_logger_server),
    meck:expect(nn_logger_server, log, fun(_, _, _, _) -> ok end),
    meck:new(nn_sub_supervisor),
    meck:expect(nn_sub_supervisor, start_child, 
		fun(content_handler) -> nn_content_handler:start_link()
		end).
					  
clear_test_state() ->
    meck:unload(nn_logger_server),
    meck:unload(nn_sub_supervisor),
    case filelib:is_file(
	   "files/O5w1jzbwoxtq0+FPMJx88ZiskkboMW+c5UPVsZrAK4A") of
	true ->
	    file:delete(
	      "files/O5w1jzbwoxtq0+FPMJx88ZiskkboMW+c5UPVsZrAK4A");
	false ->
	    ok
    end.

%%%===================================================================
%%% Internal tests
%%%===================================================================

setup() ->
    FN="files/Name",
    {ok, F} = file:open("files/Name", [write, binary]),
    file:write_file("files/Name", <<"file">>),
    file:close(F),
    FN.
       								
teardown(F) ->
    file:delete(F),
    F1 = "files/" ++ 
	string:strip(binary_to_list(hash_c(<<"file">>)), both, $=),
    case filelib:is_file(F1) of
	true ->
	    file:delete(F1);
	false ->
	    ok
    end.    
    
content_handler_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun fetch_file/1,
      fun store_file/1
     ]}.

fetch_file(_) ->
    GoodRet = fetch("ni:///sha-256;Name"),
    BadRet = fetch("ni:///sha-256;Bogus"),
    [
     ?_assertEqual(GoodRet, {ok, <<"file">>}),
     ?_assertMatch(BadRet, {ok, file_not_found})
    ].


store_file(_) ->
    RealHash = "ni:///sha-256;" ++  
	list_to_binary(string:strip(binary_to_list(hash_c(<<"file">>)), both, $=)),
    FalseName = "ni:///sha-256;File",
    GoodResp = store({RealHash, <<"file">>}), 
    BadResp = store({FalseName, <<"file">>}),
    [
     ?_assertEqual(GoodResp, ok),
     ?_assertMatch(BadResp, {error, invalid_hash})
    ].

hash_c(Bin) ->
      base64:encode(
	crypto:hash(sha256, Bin)).
