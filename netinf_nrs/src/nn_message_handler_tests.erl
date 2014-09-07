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
%%% @author Faroogh Hassan
%%% @doc
%%%  Unit tests for the message handler API functions.
%%% @end
%%% Created : 13 Nov 2012 by Faroogh Hassan
%%%-------------------------------------------------------------------
-module(nn_message_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% nn_message_handler:set_test_state(),
    %% {ok, Pid} = nn_message_handler:spawn(),
    %% Pid.
    ok.
								
teardown(_) ->
    ok.
    %% nn_message_handler:kill(Pid),
    %% nn_message_handler:clear_test_state().
    
event_handler_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun pubreq/1,
      fun getreq/1
     
     ]}.

pubreq(_)->
    [?_assertEqual(1,0)].
%%     HTTPpub=
%% 	"{http_req, #Port<0.2377>, ranch_tcp, keepalive,<0.436.0>, <<\"POST\">>,
%% {1,1},  undefined,<<\"localhost\">>, undefined, 9999,
%%  <<\"/netinfproto/publish\">>,  undefined,<<>>,   undefined,
%%   <<>>,[],  [{<<\"user-agent\">>,
%%  <<\"curl/7.22.0 (i686-pc-linux-gnu) 
%% 	libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3\">>},
%%  {<<\"host\">>, <<\"localhost:9999\">>}, {<<\"accept\">>, <<\"*/*\">>},
%%  {<<\"content-type\">>,
%%  <<\"multipart/form-data; 
%% boundary=------WebKitFormBoundaryjTwy4nYi2Aj6shCW\">>},
%%  {<<\"content-length\">>, <<\"879\">>}], [],undefined,[],waiting, undefined,
%%  <<\"--------WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition: 
%% form-data; name=\"URI\"\r\n\r\nni:///sha-256;
%% d748c318b3f2e637163693738b6b80bcf7fb4003f51d390c8367c8d32705df81\r\n--------
%% WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition: form-data;
%%  name=\"msgid\"\r\n\r\n1234\r\n--------
%% WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition: 
%% form-data; name=\"ext\"\r\n\r\n{\"meta\":{\"key\":\"banana\"}}\r\n--------
%% WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\n
%% Content-Disposition: form-data; name=\"octets\"; 
%% filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n
%% --------WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition:
%%  form-data; name=\"loc1\"\r\n\r\nsomewere\r\n--------
%% WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition:
%%  form-data; name=\"loc2\"\r\n\r\nelsewere\r\n--------
%% WebKitFormBoundaryjTwy4nYi2Aj6shCW\r\nContent-Disposition: 
%% form-data; name=\"rform\"\r\n\r\njson\r\n-------
%% -WebKitFormBoundaryjTwy4nYi2Aj6shCW--\r\n\">>,  waiting,[], <<>>,
%%  undefined}",

%% {ok, Replypub} = nn_message_handler:handle_request(Pid, HTTPpub),
%% [
%%  ?_assertEqual(Replypub,HTTPpub)
%% ].


getreq(_)->
    [?_assertEqual(1,0)].
%%     HTTPget=
%% 	"{http_req, #Port<0.2355>, ranch_tcp,  keepalive,  <0.410.0>,  <<\"POST\">>,
%%   {1,1},  undefined, <<\"localhost\">>,   undefined,9999,
%%  <<\"/netinfproto/get\">>, undefined,<<>>,undefined,<<>>,  [],
%%  [{<<\"user-agent\">>,
%%  <<\"curl/7.22.0 (i686-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 
%% 	zlib/1.2.3.4 libidn/1.23 librtmp/2.3\">>},  {<<\"host\">>,
%%   <<\"localhost:9999\">>}, {<<\"accept\">>, <<\"*/*\">>}, 
%% {<<\"content-length\">>, <<\"98\">>},  {<<\"content-type\">>, 
%% <<\"application/x-www-form-urlencoded\">>}], [], 
%% undefined,[], waiting,   undefined, <<\"URI=ni:///sha-256;
%% allll318b3f2e637163693738b6b80bcf7fb4003f51d390c8367c8d32705df81
%% &msgid=1337&ext=\">>, waiting,[],<<>>,   undefined}}",


%% {ok, Replyget} = nn_message_handler:handle_request(Pid, HTTPget),
%% [
%%  ?_assertEqual(Replyget, HTTPget)
%% ].
