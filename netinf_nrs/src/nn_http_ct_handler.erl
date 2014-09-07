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
%%%  Handles incoming content transfer requests.
%%% @end
%%% Created :  3 Dec 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(nn_http_ct_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

%%--------------------------------------------------------------------
%% @doc
%% Initializes the http content transfer handler
%% @end
%%--------------------------------------------------------------------
init({tcp, http}, Req, _) ->
    {ok, Req, undefined_state}.

%%--------------------------------------------------------------------
%% @doc
%% Spawns the process for content transfer handler and forwards the request 
%% to content transfer handler. 
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {ok, Pid} = nn_ct_handler:spawn(),
    {ok, Status, CtReq} = nn_ct_handler:handle_request(Pid, Req),
    {ok, ReplyReq} = cowboy_req:reply(Status, CtReq),
    nn_ct_handler:kill(Pid),
    {ok, ReplyReq, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates the http content transfer handler 
%% @end
%%--------------------------------------------------------------------
terminate(_, _) ->
    ok.
