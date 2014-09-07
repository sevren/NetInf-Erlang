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
%%%
%%% @end
%%% Created :  3 Dec 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(nn_ct_handler).

-behaviour(gen_server).

%% API
-export([
	 start_link/0, 
	 spawn/0, 
	 kill/1,
	 handle_request/2,
	 handle_put/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Spawns a child process for the content handler.
%% @end
%%--------------------------------------------------------------------
-spec spawn() -> {ok, pid()}.
spawn() ->
    nn_sub_supervisor:start_child(ct_handler).

%%----------------------------------------------------------------------
%% @doc
%%  Kills the process given as argument.
%% @end
%%----------------------------------------------------------------------
-spec kill(pid()) -> ok.
kill(Pid) ->
    gen_server:cast(Pid, kill).
    
%%--------------------------------------------------------------------
%% @doc
%%  Handles an incoming content transfer request.
%% @end
%%--------------------------------------------------------------------
-spec handle_request(pid(), cowboy_req:req()) -> 
			    {ok, integer(), cowboy_req:req()}.
handle_request(Pid, Req) ->
    gen_server:call(Pid, {handle_request, Req}).

%%--------------------------------------------------------------------
%% @doc
%%  Handles content put request.
%% @end
%%--------------------------------------------------------------------
-spec handle_put(pid(), cowboy_req:req()) -> 
			    {ok, integer(), cowboy_req:req()}.
handle_put(Pid, Req) ->
    gen_server:cast(Pid, {handle_put, Req}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, undefined, []).

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
init(_) ->
    {ok, undefined}.

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
handle_call({handle_request, Req}, _, State) ->
    Reply = handle_content_transfer(Req),
    {reply, Reply, State}.

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
handle_cast({handle_put, Req}, State) ->
    put_data(Req),
    {noreply, State};
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

handle_content_transfer(Req) ->
    case cowboy_req:get(path, Req) of
	<<"/octets/", Name/binary>> ->
	    Pid = nn_content_handler:spawn(),
	    case nn_content_handler:fetch_content(
		   Pid, cowboy_http:urldecode(Name)) of
		{ok, file_not_found} ->
		    Response = {ok, 404, Req};
		{ok, Octets} -> 
		    Response = create_http_resp(Octets, Req)
	    end,
	    nn_content_handler:kill(Pid),
	    Response;
	_ ->
	    {ok, 404, Req}
    end.

put_data(Req) ->
    put_data(Req, undefined, undefined).
put_data(Req, Name, Data) ->
    case cowboy_req:multipart_data(Req) of
	{headers, Header, HeaderReq} ->
	    case get_header_type(Header) of
		{ok, octets} ->
		    {BodyReq, Octets} = nn_http_formatting:assemble_body(HeaderReq),
		    put_data(BodyReq, Name, Octets);
		{ok, chunk_name} ->
		    {BodyReq, ChunkName} = nn_http_formatting:assemble_body(HeaderReq),
		    put_data(BodyReq, ChunkName, Data);
		nomatch ->
		    {ok, ReplyReq} = cowboy_req:multipart_skip(Req),
		    put_data(ReplyReq, Name, Data)
	    end;
	{end_of_part, EoPReq} ->
	    put_data(EoPReq, Name, Data);
	{eof, _} ->
	    CH = nn_content_handler:spawn(),
	    nn_content_handler:store_content_without_validating(CH, Name, Data),
	    nn_content_handler:kill(CH)
    end.

get_header_type([])->
    nomatch;
get_header_type([{_, Head} | Tail]) -> 
    case binary:split(Head, <<"=">>) of
	[_, <<"\"chunkName\"">>]->
	    {ok, chunk_name};
	[_, <<"\"octets\"", _/binary>>] ->
	    {ok, octets};		
	_ ->
	    get_header_type(Tail)
    end.

create_http_resp(Octets, Req) ->
    HeaderKey = <<"Content-Type">>, 
    HeaderValue = <<"application/octet-stream">>,
    HeadReq = cowboy_req:set_resp_header(HeaderKey, HeaderValue, Req),
    BodyReq = cowboy_req:set_resp_body(Octets, HeadReq),
    {ok, 200, BodyReq}.
