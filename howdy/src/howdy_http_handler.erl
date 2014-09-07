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
-export([determine_action/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, undefined) ->
    handle(Req, []);

handle(Req, State) ->
   % io:format("Got this message: ~p~n",[Req]),
    {Method, Req} = cowboy_req:method(Req),
   % {_, Header, Req2} = cowboy_req:multipart_data(Req),
   % io:format("First of multipart ~p~n", [Header]),
   % {_, Body, _Req3} = cowboy_req:multipart_data(Req2),
   % io:format("Hopefully some kind of body ~p~n", [Body]),
    determine_action(Method, [<<"echo">>, <<"io">>], Req, State).
    
determine_action(Method,ParamList,Req,State) ->
    case is_netinf_req(Req) of
	{ok, get} ->
	    {ok, Req} = handle_get(Req,State),
	    {ok, Req, State};
	{ok, publish} ->
	   {ok, ReplyReq} = cowboy_req:reply(200, Req),
	    {ok, ReplyReq, State};
						%handle_publish(Req,State);
	{ok, false} ->
	    {ok, ResponseReq} = determine_action(Method,ParamList,Req),
	    {ok, ResponseReq, State}
    end.
determine_action(<<"GET">>, [], Req) ->
    cowboy_req:reply(400, [], <<"Invalid parameter.">>, Req);
determine_action(<<"GET">>, [Param | T], Req) ->
    case cowboy_req:qs_val(Param, Req) of
	{undefined, ResponseReq} ->
	    determine_action(<<"GET">>, T, ResponseReq);
	{Value, ResponseReq} ->
	    handle_request(Param, Value, ResponseReq)
    end;
determine_action(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

handle_request(<<"echo">>, Echo, Req) ->
    cowboy_req:reply(200,
		     [{<<"Content-Encoding">>, <<"utf-8">>}],Echo, Req);
handle_request(<<"io">>, Io, Req) ->
    cowboy_req:reply(200,
		     [{<<"Content-Encoding">>, <<"utf-8">>}], gen_io(Io), Req).

handle_get(Req,_State) ->
    io:format("NetInf GET Request Body~n\t~p", [cowboy_req:get(buffer, Req)]),
    cowboy_req:reply(200,
		    [{<<"Content-Type">>, <<"application/json">>},{<<"Content-Encoding">>, <<"utf-8">>}],
		    <<"[\"nimacbt:\\/\\/9C:02:98:FF:F3:58\"]">>, Req).

handle_publish(Req, State) ->
    case cowboy_req:multipart_data(Req) of
	{headers, [{_,<<"form-data; name=\"URI\"">>}|_], HeaderReq} ->
	    {HeaderReq, BodyData} = parse_body(Req),
	    handle_publish(HeaderReq, [{BodyData,[]}|State]);
	{headers, [{_,Header}|_], HeaderReq} ->
	    case binary:match(Header, <<"loc">>) of
		nomatch ->
		    {HeaderReq, _} = parse_body(Req),
		    handle_publish(HeaderReq, State);
		_ ->
		    {HeaderReq, BodyData} = parse_body(Req),
		    {PropKey, PropData} = hd(State),
		    handle_publish(HeaderReq, [{PropKey,[BodyData|PropData]}|State])
	    end;
	{end_of_part, EndOfPartReq} ->
	    handle_publish(EndOfPartReq, State);
	{eof, EoFReq}  ->
	    {ok, ReplyReq} = cowboy_req:reply(200,EoFReq),
	    {ok, ReplyReq, State}
    end.

gen_io(C) ->
    random:seed(now()),
    BinaryAsList = binary_to_list(C),
    FilteredList = lists:filter(fun(X) -> X >= $0 andalso X =< $9 end,BinaryAsList),
    Number = list_to_integer(FilteredList),
    RandomNumber = random:uniform(Number),
    io:format("Generating random number X where  1 <= X <= ~p ... ~n
               Generated ~p~n
               ...~n
               Sending random number...~n", [Number,RandomNumber]),
    list_to_binary(integer_to_list(RandomNumber)).

terminate(_Req, _State) ->
	ok.

is_netinf_req(Req) ->
    case cowboy_req:get(path,Req) of
	<<"/.well-known/netinfproto/get">> ->
	    {ok, get};
	<<"/.well-known/netinfproto/publish">> ->
	    {ok, publish};
	_ ->
	    {ok, false}
    end.

parse_body(Req) ->
    parse_body(Req,[]).

parse_body(Req,Ack) ->
    case cowboy_req:multipart_data(Req) of
	{body, BodyBinary, BodyReq} ->
	    parse_body(BodyReq,Ack ++ binary_to_list(BodyBinary));
	_ ->
	    {Req,list_to_binary(Ack)}
    end.
