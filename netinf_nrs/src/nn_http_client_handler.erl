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
%%% @author Daniele Bacarella
%%% @author Alexander Lindholm
%%% @doc
%%% This module is an HTML interface for Netinf Get, Publish and Search   
%%% requests. It forwards these requests to the local NRS.  
%%% @end
%%% Created : 27 Nov 2012 by Daniele <db@belladonna>
%%%-------------------------------------------------------------------
-module(nn_http_client_handler).

-include_lib("eunit/include/eunit.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

%%--------------------------------------------------------------------
%% @doc
%% initializes the http client handler
%% @end
%%--------------------------------------------------------------------
-spec init({atom(), atom()}, cowboy_req:req(), _) 
	  -> {ok,  cowboy_req:req(), undefined_state}.
init({tcp, http}, Req, _) ->
    {ok, Req, undefined_state}.

%%--------------------------------------------------------------------
%% @doc
%% Handles an HTTP request and determines if it is a full put or not. 
%% It also extracts the path in a Netinf request and performs appropriate 
%% action.  
%% @end
%%--------------------------------------------------------------------

handle(Req, State) ->
    Headers = cowboy_req:get(headers, Req),        
    case is_fullput(Headers) of 
     	true ->
    	    nn_logger_server:log(verbose, ?MODULE, handle,
				 "FULLPUT IS TRUE");
     	false ->
     	    nn_logger_server:log(verbose, ?MODULE, handle,
				 "FULLPUT IS FALSE")
    end,
    
    {ok, Pid} = spawn_msghandler(),
    
    case cowboy_req:get(path, Req) of 
	<<"/favicon.ico">> ->
	    nn_logger_server:log(verbose, ?MODULE, handle,
				 "skipping FAVICON , return 404"),
	    {ok, ReplyReq} = cowboy_req:reply(404, Req);
	<<"/stats">> ->
	    {ok, ReplyReq} = cowboy_req:reply(200, 
					      [],
					      get_stats(),
					      Req);
	<<"/stream">> ->
	    {ok, CtPort} = application:get_env(netinf_nrs, ct_port),
	    Ip = nn_discovery_service:get_ipaddr(),
            StreamUrl = "<script>var streamUrl = '" 
		++ Ip 
		++ ":" 
		++ integer_to_list(CtPort) 
		++ "';</script>\n",
	    {ok, ReplyReq} = cowboy_req:reply(200, [], 
						generate_page("./resources/streamheader.html")
                                                ++ StreamUrl    
					        ++ generate_page("./resources/stream.html")
						++ generate_page("./resources/middle.html") 
						++ generate_page("./resources/bottom.html"), 
						Req);
	<<"/streampure">> ->
	    {ok, CtPort} = application:get_env(netinf_nrs, ct_port),
	    Ip = nn_discovery_service:get_ipaddr(),
            StreamUrl = "<script>var streamUrl = '" 
		++ Ip 
		++ ":" 
		++ integer_to_list(CtPort) 
		++ "';</script>\n",
	    {ok, ReplyReq} = cowboy_req:reply(200, [], 
						generate_page("./resources/streamheader.html")
                                                ++ StreamUrl    
					        ++ generate_page("./resources/streampure.html")
						++ generate_page("./resources/middle.html") 
						++ generate_page("./resources/bottom.html"), 
						Req);
	<<"/cache">> ->
	    cache_stuff(Req),
	    {ok, ReplyReq} = cowboy_req:reply(200, Req);
	<<"/resources", File/binary>> ->
	    {ok, ReplyReq} = cowboy_req:reply(200, [], 
						generate_page("./resources/" ++ binary_to_list(File)), 
						Req);
	<<"/subscribe", Rest/binary>> ->
	    subscribe(Req, Rest),
	    {ok, ReplyReq} = cowboy_req:reply(200, [], 
					        <<"{'status':'ok'}">>,
					      Req);
        <<"/searchpure">> ->
   	    ReplyReq = check_req_json(Pid, 
				      cowboy_req:set([{path, <<"/netinfproto/search">>}], Req)
				     );
	_ ->
	    ReplyReq = check_req(Pid, Req)
    end,
    nn_message_handler:kill(Pid),
    {ok, ReplyReq, State}.     
    
subscribe(Req, StreamType) ->
    io:format("subscribe stream type ~p~n", [StreamType]),
    Buffer = cowboy_http:urldecode(cowboy_req:get(buffer, Req)),
    IP = binary_to_list(uri_split(Buffer, <<"ip=">>)),  
    Ni = binary_to_list(uri_split(Buffer, <<"ni=">>)),
    nn_logger_server:log
      (verbose, ?MODULE, subscribe, ["Subscribe ", IP, Ni]),
    case binary:match(StreamType, <<"search_and_get">>) of
	nomatch ->
	    StreamFun = no_search_get;
	_ ->
	    StreamFun = search_and_get
    end,
    nn_stream_handler:start_stream(Ni, IP, StreamFun).
    
uri_split(Buffer, Element) ->
    [_, URIFirstSplit] = binary:split(Buffer, Element),
    hd(binary:split(URIFirstSplit, <<"&">>)).  
  
get_stats() ->
    {ok, Fetch} = nn_stats:fetch(),
    {ok, Json} = json:encode(Fetch),
    Json.

is_fullput(Headers) ->
    case proplists:get_value(<<"content-type">>, Headers) of
	<<"multipart/form-data;", _/binary>> ->
	    true;
	_ -> 
	    false
   end.

check_req_json(Pid, Req) ->
    case forward_msghandler(Pid, Req) of
	  {ok, {400, _}} ->
	      {ok, ReplyReq} = cowboy_req:reply(200, [], "", Req),
	      nn_logger_server:log(verbose, ?MODULE, handle,
				   "generate frontpage");
	  {ok, {_, Req2}} ->
	      nn_logger_server:log(verbose, ?MODULE, handle,
				   ["generate page for search/get/publish response"]),
	      Buffer = cowboy_req:get(resp_body, Req2),
	      {ok, ReplyReq} = cowboy_req:reply(200, [],
						Buffer,
						Req)
      end,
    ReplyReq.

check_req(Pid, Req)->
      case forward_msghandler(Pid, Req) of
	  {ok, {400, _}} ->
	      {ok, ReplyReq} = cowboy_req:reply(200, [], 
						generate_page("./resources/header.html") 
						++ generate_page("./resources/middle.html")  
						++ generate_page("./resources/bottom.html"), 
						Req),
	      nn_logger_server:log(verbose, ?MODULE, handle,
				   "generate frontpage");
	  {ok, {_, Req2}} ->
	      nn_logger_server:log(verbose, ?MODULE, handle,
				   ["generate page for search/get/publish response"]),
	      Buffer = cowboy_req:get(resp_body, Req2),
	      {ok, ReplyReq} = cowboy_req:reply(200, [],
						generate_page("./resources/header.html") 
						++ binary_to_list(Buffer)
						++ generate_page("./resources/middle.html")
						%++ binary_to_list(get_stats())
						++ generate_page("./resources/bottom.html"), 
						Req)
      end,
    ReplyReq.

%%@doc tear down the connection
terminate(_, _) ->
    ok.
%%%=====================================================
%%% Internal functions
%%%=====================================================

cache_stuff(Req) ->
    {ok, Pid} = nn_ct_handler:spawn(),
    nn_ct_handler:handle_put(Pid, Req).
    
%%@doc creates frontpage
generate_page(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try 
	get_all_lines(Device)
    after 
	file:close(Device)
    end.


get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.


%%@doc spawns a msg handler
spawn_msghandler()->
    {ok, Pid}= nn_message_handler:spawn(nn_http_formatting),
    nn_logger_server:log(verbose, ?MODULE, spawn_msgHandler,
				 {ok, Pid, "Has been spawned"}),
    {ok, Pid}.
   

%%@doc forwards the message recieved from handle to the spawned msg handler
%% and waits for the reply
forward_msghandler(Pid, Req)->
    nn_message_handler:handle_request(Pid, Req),
    receive
	{ok, Pid, ReplyReq} ->
	    nn_logger_server:log(verbose, ?MODULE, forward_msgHandler,
				 "Has been forwarded"),
	    ReplyReq
    end.
	 
