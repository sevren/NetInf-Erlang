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
%%% @author Thomas Nordstrom
%%% @author Marcus Ihlar
%%% @doc
%%%  This module contains utility functions that are used in several other 
%%%  modules 
%%% @end
%%% Created : 27 Nov 2012 by Thomas

-module(nn_util).

-include_lib("eunit/include/eunit.hrl").

-export([
	 safe_list_to_integer/1,
	 time_left/2, 
	 create_timestamp/0,
	 decode/1, 
	 decode/2,
	 bin_replace/3,
	 shuffle_list/1,
	 get_ipaddr/0,
	 n_byte_binary/1,
	 unique_id/0
	]).

%%--------------------------------------------------------------------
%% @doc
%%  Does a list to integer but will return nicely insted of crashing 
%%  in case of bad input
%% @end
%%--------------------------------------------------------------------

-spec safe_list_to_integer(string()) -> 
				  {ok, integer()} | {ok, not_integer}.
safe_list_to_integer(L) ->
    safe_list_to_integer(L, 0).

safe_list_to_integer([], Acc) ->
    {ok, Acc};
safe_list_to_integer([Hd | Tl], Acc) when Hd >= $0 andalso Hd =< $9 ->
    safe_list_to_integer(Tl, Acc * 10 + Hd - $0);
safe_list_to_integer(_, _) ->
    {ok, not_integer}.


%%--------------------------------------------------------------------
%% @doc
%%  Returns the time that is left or zero if the time has run out
%% @end
%%--------------------------------------------------------------------

-spec time_left(integer(), integer()) -> integer().
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
	Time when Time =< 0 -> 0;
	Time -> Time * 1000
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Creates a formated timestamp
%% @end
%%--------------------------------------------------------------------

-spec create_timestamp() -> string().
create_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = erlang:localtime(),
    list_to_binary(
      [
       padded_int(Y), $-, padded_int(M), $-, padded_int(D), 
       $T, padded_int(H), $:, padded_int(Mi), $:, 
       padded_int(S), "+00:00"
      ]).

padded_int(N) when N < 10 ->
    [$0 | integer_to_list(N)];
padded_int(N) ->
    integer_to_list(N).

%%--------------------------------------------------------------------
%% @doc
%%  Returns a proplist representing the json object if it was valid
%%  otherwise an empty list
%% @end
%%--------------------------------------------------------------------

-spec decode(string() | binary()) -> [{term(), term()}].
decode(Body) ->
    case json:decode(Body) of
	{ok, {Resp}} ->
	    Resp;
	_ ->
	    []
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Returns a proplist representing the json object with the octets
%%  added to it if the json was valid otherwise just the octets
%% @end
%%--------------------------------------------------------------------
-spec decode(string(), string() | binary()) -> [{term(), term()}].
decode(Boundary, Body) ->
    [_, Json, Binary, _] = re:split(Body, Boundary),
    CleanJson = get_within(binary_to_list(Json), ${, $}),
    CleanBinary = get_binary(Binary),
    JsonList = decode(CleanJson),
    [{<<"octets">>, CleanBinary} | JsonList].

get_within([Opening | Tl], Opening, Closing) ->
    get_within(lists:reverse([Opening | Tl]), Opening, Closing);
get_within([Closing | Tl], _, Closing) ->
    lists:reverse([Closing | Tl]);
get_within([_ | Tl], Opening, Closing) ->
    get_within(Tl, Opening, Closing);
get_within([], _, _) ->
    [].

get_binary(Bin)->
    StripedBin = re:replace(Bin, "^.+octet-stream\r\n\r\n", "", 
			    [dotall, {return, binary}]),
    re:replace(StripedBin, "(\r\n|\r\n--)$", "", [{return, binary}]).


%% strip_bin([])->
%%     [];
%% strip_bin("octet-stream" ++ Tl) ->
%%     Tl;
%% strip_bin([_|Tl]) ->
%%     strip_bin(Tl).

%% strip_bin(<<>>) ->
%%     <<>> ;
%% strip_bin(<<"octet-stream\r\n\r\n", Rest/binary>>) ->
%%     Rest;
%% strip_bin(<<_, Rest/binary>>) ->
%%     strip_bin(<<Rest/binary>>).



%%--------------------------------------------------------------------
%% @doc
%%  Shuffles the <em>List</em>
%%  Taken from http://www.trapexit.org/RandomShuffle
%% @end
%%--------------------------------------------------------------------
-spec shuffle_list(list()) -> list().
shuffle_list(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.

%%--------------------------------------------------------------------
%% @doc
%%  Replaces one character in the binary with another
%% @end
%%--------------------------------------------------------------------

-spec bin_replace(binary(), binary(), binary()) -> binary().
bin_replace(Bin, Old, New) ->
    bin_replace(Bin, <<>>, Old, New).
bin_replace(<<>>, Acc, _, _) ->
    Acc;
bin_replace(<<Old, Rest/binary>>, Acc, Old, New) ->
    bin_replace(Rest, <<Acc/binary, New>>, Old, New);
bin_replace(<<First, Rest/binary>>, Acc, Old, New) ->
    bin_replace(Rest, <<Acc/binary, First>>, Old, New).

%%--------------------------------------------------------------------
%% @doc
%%  Returns the parsed ipv4 or ipv6 from the system in erlang format. 
%% @end
%%--------------------------------------------------------------------

-spec get_ipaddr() -> string().   
get_ipaddr()->
 {ok, Ipaddrs} = inet:getif(),
    {Ip, _, _} = hd(Ipaddrs),    
    inet_parse:ntoa(Ip).

%%--------------------------------------------------------------------
%% @doc
%%  Generates a random binary of N bytes.
%% @end
%%--------------------------------------------------------------------
-spec n_byte_binary(integer()) -> binary().
n_byte_binary(N) ->
    n_byte_binary(N, <<>>).

n_byte_binary(0, Binary) ->
    Binary;
n_byte_binary(N, <<Acc/binary>>) ->
    Val = random:uniform(256) - 1,
    n_byte_binary(N-1, <<Acc/binary, Val>>).

%%--------------------------------------------------------------------
%% @doc
%%  Returns a string that is guaranteed to be unique within the system.
%% @end
%%--------------------------------------------------------------------
unique_id() ->
    {_, _, Micros} = now(),
    integer_to_list(Micros).

%%====================================================================
%% Internal Test
%%====================================================================

setup() ->
    ok.
run_test_() ->
    {setup,
     fun setup/0,
     fun(Sup) ->
	     [
	      test_safe_list_to_integer(Sup),
	      test_decode(Sup), 
	      test_bin_replace(Sup)
	     ]
     end
    }.

test_safe_list_to_integer(_) ->
    {ok, RecievedInt} = safe_list_to_integer("123"),
    {ok, RecievedError} = safe_list_to_integer("12e3"),
    [
     ?_assertEqual(123, RecievedInt),
     ?_assertEqual(not_integer, RecievedError)
    ].
    

test_decode(_) ->
    JsonString = "{\"a\":1}",
    ExpectedJsonObject = [{<<"a">>,1}],
    ExpectedBinary = [{<<"octets">>, <<"test">>},
		      {<<"a">>, 1}],
    RecivedJson = decode(JsonString),
    Boundery = "----------------------------83ff53821b7c",
    MultipartString = 
	"--" ++ Boundery ++ "\r\n" ++
	"Content-Disposition: form-data; name=\"json\"\r\n" ++
	"Content-Type: application/json\r\n\r\n" ++
	JsonString ++ "\r\n" ++
	"--" ++ Boundery ++ "\r\n" ++
	"Content-Disposition: form-data; name=\"octets\"\r\n" ++
	"Content-Type: application/octet-stream\r\n\r\n" ++
	"test\r\n" ++
	"--" ++ Boundery ++ "--\r\n",
    RecivedBinary = decode(Boundery, MultipartString),
    [
     ?_assertEqual(ExpectedJsonObject, RecivedJson),
     ?_assertEqual(ExpectedBinary, RecivedBinary)
    ].
    
	

test_bin_replace(_) ->
    OriginalBin = <<"hej alla glada">>,
    ReplacedBin = bin_replace(OriginalBin, $ , $p),
    ShouldBeUnchangedBin = bin_replace(OriginalBin, $q, $p),
    [
     ?_assertEqual(OriginalBin,ShouldBeUnchangedBin),
     ?_assertEqual(<<"hejpallapglada">>, ReplacedBin)
    ].
