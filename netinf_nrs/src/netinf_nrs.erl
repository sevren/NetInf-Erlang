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
%%%  Use this module to start the netinf system.
%%% @end
%%% Created : 19 Oct 2012 by Marcus Ihlar
%%%-------------------------------------------------------------------
-module(netinf_nrs).

-export([start/0, stop/0]).


%%--------------------------------------------------------------------
%% @doc
%% Starts Ranch, Crypto, Cowboy and Netinf NRS 
%% @end
%%--------------------------------------------------------------------

-spec start() -> ok.
start() ->
    _ = inets:start(),
    _ = httpc:set_options([{max_sessions, 0},
			   {max_keep_alive_length, 0},
			   {keep_alive_timeout, 0}]),
    ok = application:start(ranch),
    _ = application:start(crypto),
    ok = application:start(cowboy),
    ok = application:start(netinf_nrs).


%%--------------------------------------------------------------------
%% @doc
%% Stops Ranch, Crypto, Cowboy and Netinf NRS  
%% @end
%%--------------------------------------------------------------------

-spec stop() -> ok.
stop() ->
    inets:stop(),
    ok = application:stop(netinf_nrs),
    ok = application:stop(cowboy),
    ok = application:stop(crypto),
    ok = application:stop(ranch).
