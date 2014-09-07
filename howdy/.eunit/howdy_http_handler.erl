%%% @author Marcus Ihlar <marcus@ihlar.se>
%%% 
%%% @doc
%%% This is a simple http servers that responds to GET-requests. 
%%% Based on the example echo_get from the Cowboy project.
%%% 
%%% Two types of parameter are handeled, echo and io. The first
%%% one is a simple echo of the request value. The other returns
%%% a static string.
%%% @end
%%% Created :  4 Oct 2012 by Marcus Ihlar <marcus@ihlar.se>

-module(howdy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Bin, _R} = cowboy_req:qs(Req2),
    io:format("~p~n", [binary_to_list(Bin)]),
    {ok, Req3} = determine_action(Method, [<<"echo">>, <<"io">>], Req2),
    {ok, Req3, State}.
    
determine_action(<<"GET">>, [], Req) ->
    cowboy_req:reply(400, [], <<"Invalid parameter.">>, Req);
determine_action(<<"GET">>, [Param | T], Req) ->
    case cowboy_req:qs_val(Param, Req) of
	{undefined, Req2} ->
	    determine_action(<<"GET">>, T, Req2);
	{Value, Req2} ->
	    handle_request(Param, Value, Req2)
    end;
determine_action(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

handle_request(<<"echo">>, Echo, Req) ->
    cowboy_req:reply(200,
		     [{<<"Content-Encoding">>, <<"utf-8">>}], Echo, Req);
handle_request(<<"io">>, Io, Req) ->
    cowboy_req:reply(200,
		     [{<<"Content-Encoding">>, <<"utf-8">>}], gen_io(Io), Req).

gen_io(C) ->
    list_to_binary("I am an IO, do not comment on my BO! " ++ C).

terminate(_Req, _State) ->
	ok.

