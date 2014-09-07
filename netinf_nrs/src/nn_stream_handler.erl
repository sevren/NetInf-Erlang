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
%%% @author Marcus Ihlar
%%% @doc
%%%  Handles fetching and polling of chunked octets
%%% @end
%%% Created :  6 Dec 2012 by Jon Borglund
%%%-------------------------------------------------------------------
-module(nn_stream_handler).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
	 start_link/0,
	 start_stream/3
	]).

%% Time in seconds.
-define(DEFAULT_LOC_LEASE, 5).
-define(DEFAULT_CHUNK_LEASE, 1).

%% Utterly arbitrary value...
-define(MAXATTEMPTS, 120).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% fields specific for no search, get type of streaming 
-record(no_search_get_record,
	{
	  stream_name = undefined :: string(),
	  loc_start_time = undefined :: undefined | non_neg_integer(),
	  loc_lease_time = undefined :: undefined | non_neg_integer()
	}).

%% fields common to all types of streaming
-record(state, 
	{
	  fun_specific_params = undefined :: undefined | #no_search_get_record{},
	  stream_fun = undefined :: undefined | 
				    fun ((#state{}) -> 
						{noreply, #state{}, non_neg_integer()}),
	  chunk_start_time = undefined :: undefined | non_neg_integer(),
	  chunk_lease_time = undefined :: undefined | non_neg_integer(),
	  chunk_nr = undefined :: undefined | non_neg_integer(),
	  not_found_count = 0 :: non_neg_integer(), 
	  nrs_ip = undefined :: undefined | binary(),
	  locators = undefined :: undefined | [binary()],
	  name = undefined :: undefined | string()  
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the stream
%% @end
%%--------------------------------------------------------------------
-spec start_stream(Name::binary(), 
		   NrsHttpIp::binary(), 
		   StreamFun::atom()) -> 
			  ok.
start_stream(URI, NrsIp, StreamFun) ->
    {ok, Pid} = nn_client_supervisor:start_child(
		  stream_handler, []),
    gen_server:call(Pid, {setup_stream, URI, NrsIp, StreamFun}).

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
  {ok, #state{}}.

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
handle_call({setup_stream, URI, NrsIp, StreamFun}, _From, State) ->
    nn_logger_server:log(verbose, ?MODULE, handle_call,
			  ["trying to start stream of type ", StreamFun]),
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    NewState = State#state{
		 name = URI,
		 nrs_ip = NrsIp,
		 chunk_start_time = StartTime,
		 chunk_lease_time = 0,
		 chunk_nr = 1
		},
    
    case StreamFun of
	search_and_get ->			 
	    Fun = fun search_and_get/1,
	    {reply, {ok, stream_started}, NewState#state{
					    stream_fun = Fun, 
					    name = URI}, 0} ;
	no_search_get ->
	    {_, Hash} = nn_hash_validation:parse_name(URI),
	    case nn_subscribe:subscribe(URI, NrsIp) of
		{error, _} ->
		    {stop, normal, {error, no_stream}, State} ;
		{ok, Locs} ->
		    Fun = fun no_search_get/1,
		    Name = "ni:///demo;" ++ binary_to_list(Hash),
		    NoSearchRec = #no_search_get_record{
		      stream_name = Name,
		      loc_start_time = StartTime,
		      loc_lease_time = ?DEFAULT_LOC_LEASE
		     },
		    
		    RetState = NewState#state{
				 fun_specific_params = NoSearchRec,
				 stream_fun = Fun,
				 locators = Locs
				},
		    {reply, {ok, stream_started}, RetState, 0}
	    end
    end.
				 
%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{stream_fun = StreamFun}) ->
    StreamFun(State).
	    
%%--------------------------------------------------------------------
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

no_search_get(State) ->
    #state{
	    fun_specific_params = NoSearchParams,
	    chunk_start_time = Cstart,
	    chunk_lease_time = Clease,
	    nrs_ip = NRSIP,
	    chunk_nr = ChunkNr,
	    locators = Locators
	  } = State,
    #no_search_get_record{
		loc_start_time = LocStartTime,
		loc_lease_time = LocLeaseTime
	       } = NoSearchParams,
    case nn_util:time_left(Cstart, Clease) <
	nn_util:time_left(LocStartTime, LocLeaseTime) of
	true ->	
	    URI = NoSearchParams#no_search_get_record.stream_name,
	    FetchChunkResp = fetch_chunk(Locators, 
					 URI ++ integer_to_list(ChunkNr),
					 fun store/2),
	    Now = calendar:local_time(),
	    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
	    Timeout = get_min_timeout(CurrentTime,
				      Clease,
				      LocStartTime,
				      LocLeaseTime),
	    
	    case FetchChunkResp of
		{ok, stored} ->
		    NewChunkNr = State#state.chunk_nr + 1,
		    {noreply, State#state{not_found_count = 0,
					 chunk_start_time = CurrentTime,
					 chunk_nr = NewChunkNr}, Timeout};
		{ok, not_found} when State#state.not_found_count >= 
				     ?MAXATTEMPTS ->
		    {stop, normal, State};
		{ok, not_found} ->
		    NewNotFoundCount = State#state.not_found_count + 1,
		    {noreply, State#state{not_found_count = NewNotFoundCount,
					  chunk_start_time = CurrentTime}, Timeout}
	    end;
	false ->
	    URI = State#state.name,
	    {ok, NewLocs} = nn_subscribe:check_locators(URI, NRSIP),
	    Now = calendar:local_time(),
	    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
	    Timeout = get_min_timeout(Cstart,
				      Clease,
				      CurrentTime,
				      LocLeaseTime),
	    NewFunArgs = NoSearchParams#no_search_get_record{
			   loc_start_time = CurrentTime},
	    {noreply, State#state{fun_specific_params = NewFunArgs,
				  locators = nn_util:shuffle_list(NewLocs)
				 }, Timeout}
    end.

get_min_timeout(Cstart, Clease, Lstart, Llease) ->
    min(nn_util:time_left(Cstart, Clease), 
	nn_util:time_left(Lstart, Llease)).

search_and_get(State) ->
    #state{
	    name = URI,
	    nrs_ip = NRSIP,
	    chunk_lease_time = Clease,
	    chunk_nr = ChunkNr
	  } = State,
    SGResp = search_and_get(NRSIP, URI, ChunkNr),
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    Timeout = nn_util:time_left(CurrentTime, Clease),
    case SGResp of
	{ok, not_found} when State#state.not_found_count >= 
			     ?MAXATTEMPTS ->
	    {stop, normal, State};
	{ok, not_found} ->
	    NewNotFoundCount = State#state.not_found_count + 1,
	    {noreply, State#state{not_found_count = NewNotFoundCount,
				  chunk_start_time = CurrentTime}, Timeout};
	{ok, stored} ->
	    NewChunkNr = State#state.chunk_nr + 1,
	    {noreply, State#state{not_found_count = 0,
				  chunk_start_time = CurrentTime,
				  chunk_nr = NewChunkNr}, Timeout}
    end.
	    
search_and_get(NRSIP, URI, ChunkNr) ->
    Token = URI ++ integer_to_list(ChunkNr),
    BaseURL = "http://"++NRSIP++":9999/netinfproto",
    SURL = BaseURL ++ "/search",
    GURL = BaseURL ++ "/get",
    PURL = BaseURL ++ "/publish",
    SearchResp = generate_search(nn_util:unique_id(), Token, [], SURL),
    Results = proplists:get_value(<<"results">>, SearchResp),
    case extract_name(Results) of 
	{ok, not_found} ->
	    {ok, not_found};
	{ok, NI} ->
	    GetResp = generate_get(NI, nn_util:unique_id(), "", GURL),
	    lists:map(fun ({K, _}) -> io:format("~p~n", [K]) end, GetResp),
	    {ok, Ext} = json:encode(proplists:get_value(<<"metadata">>, GetResp)),
	    case proplists:get_value(<<"status">>, GetResp) of
		203 ->
		    fetch_chunk(extract_locators(GetResp),
				NI, fun (Name, Octets) -> 
					     validate_and_store(Name, Octets, Ext) 
				     end),
		    publish(NI, PURL);
		200 ->
		    Octets = proplists:get_value(<<"octets">>, GetResp),
		    Name = proplists:get_value(<<"ni">>, GetResp),
		    validate_and_store(Name, Octets, Ext),
		    publish(NI, PURL);
		404 ->
		    {ok, not_found};
		Status ->
		    io:format("Got the fucked up status ~p", [Status])
	    end
    end.

extract_locators(GetResp) ->
    extract_locators(proplists:get_value(<<"loc">>, GetResp), []).

extract_locators([], Acc) ->
    nn_util:shuffle_list(Acc);
extract_locators([H|T], Acc) ->
    extract_locators(T, [binary_to_list(H) ++ "/get" | Acc]).

extract_name(undefined) ->
    {ok, not_found} ;
extract_name([]) ->
    {ok, not_found};
extract_name([{H} | T]) ->
    case proplists:get_value(<<"ni">>, H) of
	undefined ->
	    extract_name(T) ;
	NI ->
	    io:format("extracted name: ~p~n", [NI]),
	    {ok, binary_to_list(NI)}
    end.

	
fetch_chunk([], _, _) ->
    {ok, not_found};
fetch_chunk([Locator | Locators], Name, StoreFun) ->
    MSGID = nn_util:unique_id(),
    io:format("Fetch ~p from ~p~n", [Name, Locator]),
    case generate_get(Name, MSGID, "", Locator) of
	{error, _} ->
	    fetch_chunk(Locators, Name, StoreFun) ;
	Resp ->
	    case proplists:get_value(<<"status">>, Resp) of
		200 ->
		    nn_logger_server:log(verbose, ?MODULE, fetch_octets_of_chunk, 
					 ["gott stuff from", Locator]),
		    Octets = proplists:get_value(<<"octets">>, Resp),
		    StoreFun(Name, Octets);
		B ->
		    io:format("status back ~p~n", [B]),
		    fetch_chunk(Locators, Name, StoreFun)
	    end
    end.


validate_and_store(Name, Octets, Ext) ->
    LocalPubURL = "http://localhost:9999/netinfproto/publish",
    generate_publish(
      [
       {'URI', Name}, 
       {msgid, nn_util:unique_id()}, 
       {ext, Ext}, 
       {fullPut, "true"},
       {loc, ""}
      ], [{octets, Octets}], LocalPubURL).

store(Name, Octets) ->
    CH = nn_content_handler:spawn(),
    nn_content_handler:store_content_without_validating(CH, Name, Octets),
    nn_content_handler:kill(CH),
    {ok, stored}.

publish(Name, URL) ->
    MyLoc = "http://" ++ 
	nn_discovery_service:get_ipaddr() ++ ":9999/netinfproto",
    generate_publish(
      [
       {'URI', Name}, 
       {msgid, nn_util:unique_id()}, 
       {ext, ""}, 
       {loc, MyLoc}
      ], [], URL),
    {ok, stored}.

generate_get(URI, MsgID, Ext, GetURL) ->
    case httpc:request(post, 
		       {GetURL, 
			[], 
			"application/x-www-form-urlencoded",
			"URI="++ URI ++ "&msgid="++ MsgID ++ "&ext=" ++ Ext
		       }, [], []) of
	{ok, {_, HList, ResponseBody}} ->
	    case proplists:get_value("content-type", HList) of
		"multipart/form-data; boundary="++Boundary ->
		    nn_util:decode(Boundary, ResponseBody);
		_ ->
		    nn_util:decode(ResponseBody)
	    end ;
	_ ->
	    {error, bad_locator}
    end.

generate_search(MsgID, Tokens, Ext, URL)->
    io:format("~p, ~p, ~p, ~p, ~n", [MsgID, Tokens, Ext, URL]),
    {ok, {_, _, Body}} = 
	httpc:request(post, 
		      {URL, 
		       [], 
		       "application/x-www-form-urlencoded",
		       "msgid="++ MsgID ++ "&tokens=" ++ Tokens ++ "&ext=" ++ Ext
		      }, [], []),
    nn_util:decode(Body).

generate_publish(Publish, Data, URL) ->
    Boundary = "------WebKitFormBoundaryjTwy4nYi2Aj6shCW",
    Body = format_multipart_formdata(
	     Boundary, Publish, Data),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(byte_size(Body))}],
    io:format("~p~n~p~n~p~n", [Headers, ContentType, URL]),
    case httpc:request(post, {URL, Headers, ContentType, Body}, 
		       [], [{body_format, binary}]) of
	{ok, {_, [{_, <<"multipart/form-data;", Rest/binary>>}], ResponseBody}} ->
	    nn_util:decode(Rest, ResponseBody);
	{ok, {_, HList, ResponseBody}} ->
	    HList ++ nn_util:decode(ResponseBody)
    end.

format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(
		   fun({FieldName, FieldContent}) ->
			   format_one_part(FieldName, FieldContent, Boundary)
		   end, Fields),
    FieldParts2 = list_to_binary(FieldParts),
    
    FileParts = lists:map(
		  fun({FieldName, FileContent}) ->
			  format_one_part(FieldName, FileContent, Boundary)
		  end, Files),
    FileParts2 = list_to_binary(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    list_to_binary([
		    FieldParts2, FileParts2, EndingParts
		   ]).

format_one_part(octets, Binary, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(octets), "\""]),
     lists:concat([
		   "Content-Type: ", "application/octet-stream"]),
     "\r\n\r\n",
     Binary, "\r\n"];
format_one_part(FieldName, FieldContent, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(FieldName), "\""
		  ]),
     "\r\n\r\n",
     FieldContent, "\r\n"].
