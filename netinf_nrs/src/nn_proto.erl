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
%%% @author Thomas Nordström
%%% @author Kiril Goguev
%%% @author Marcus Ihlar
%%% @author Alex Lindholm
%%% @author Jon Borglund
%%% @doc 
%%% Netinf_proto is a library module for accessing and updating the 
%%% netinf protocol record.
%%% @end
%%%--------------------------------------------------------------------
-module(nn_proto).

-export([new/4, new/2, new/1, get/2, set/2]).

-record(message, 
	{
	  msgid = undefined :: term(),
	  time_to_live = undefined :: undefined | integer(),
	  octets = undefined :: undefined | binary(),
	  tokens = undefined :: undefined | [binary()],
	  method = undefined :: undefined | get | search | publish | error,
	  netinf = undefined :: undefined | proto() | [proto()] | term()
	}).

-record(netinf, {
	  % name of the ndo
	  name = undefined :: undefined | binary(),
	  % list of possible locations and or URI's
	  uri = [] ::[] | [binary()], %changed from undefined to [] by alex and jon
	  % list of extensions stored in jason format
	  ext = {[]} :: {[]} | {[{binary(), any()}]}, 
	  % timestamp of the ndo in json format
	  time_stamp = undefined :: undefined | binary() 
	 }).

-opaque proto() :: #netinf{}.
-opaque proto_msg() :: #message{}.
-export_type([proto/0, proto_msg/0]).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new netinf protocol object.
%% @end
%%--------------------------------------------------------------------
 
-spec new(binary(), [binary()], any(), binary())
	-> proto().
new(Name, URI, Ext, TimeStamp) ->
    #netinf{name = Name,
	   uri = URI,
	   ext = Ext,
	   time_stamp = TimeStamp}.

%%--------------------------------------------------------------------
%% @doc
%% Create a new netinf protocol with all fields undefined except name
%% @end
%%--------------------------------------------------------------------
-spec new(string())
	-> proto().
new(Name) ->
    #netinf{name = Name}.

%%--------------------------------------------------------------------
%% @doc
%% Create a new netinf protocol with all fields undefined except Id
%% @end
%%--------------------------------------------------------------------
-spec new(atom(), term()) -> proto().
new(msg, Id) ->
    #message{msgid = Id}.

%%--------------------------------------------------------------------
%% @doc
%% Get functions for returning specific values
%% @end
%%-------------------------------------------------------------------- 
-spec get(atom(), proto() | proto_msg()) -> any() ; 
	 ([atom()], proto() | proto_msg()) -> [any()].
get(List, Req) when is_list(List) ->
	[g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
	g(Atom, Req).

g(name, #netinf{name = Ret}) ->
    Ret;
g(uri, #netinf{uri = Ret}) ->
    Ret;
g(ext, #netinf{ext = Ret}) ->
    Ret;
g(time_stamp, #netinf{time_stamp = Ret}) ->
    Ret;
g(msgid, #message{msgid = Ret}) ->
    Ret;
g(time_to_live, #message{time_to_live = Ret}) ->
    Ret;
g(proto, #message{netinf = Ret}) ->
    Ret;
g(tokens, #message{tokens = Ret}) ->
    Ret;
g(method, #message{method = Ret}) ->
    Ret;
g(octets, #message{octets = Ret}) ->
    Ret.


%%--------------------------------------------------------------------   
%%@doc 
%%  Set functions for setting a specific value in the netinf or 
%%  message record.
%%@end
%%--------------------------------------------------------------------
-spec set([{atom(), any()}], Proto) -> 
		 Proto when Proto :: proto() | proto_msg() .
set([], Proto) -> 
    Proto;
set([{name, Val} | Tail], Proto) -> 
    set(Tail, Proto#netinf{name = Val});
set([{uri, Val} | Tail], Proto) -> 
    set(Tail, Proto#netinf{uri = Val});
set([{ext, Val} | Tail], Proto) -> 
    set(Tail, Proto#netinf{ext = Val});
set([{time_stamp, Val} | Tail], Proto) -> 
    set(Tail, Proto#netinf{time_stamp = Val});
set([{msgid, Val} | Tail], ProtoMsg) ->
    set(Tail, ProtoMsg#message{msgid = Val});
set([{time_to_live, Val} | Tail], ProtoMsg) ->
     set(Tail, ProtoMsg#message{time_to_live = Val});
set([{proto, Val} | Tail], ProtoMsg) ->
    set(Tail, ProtoMsg#message{netinf = Val});
set([{tokens, Val} | Tail], ProtoMsg) ->
    set(Tail, ProtoMsg#message{tokens = Val});
set([{method, Val} | Tail], ProtoMsg) ->
    set(Tail, ProtoMsg#message{method = Val});
set([{octets, Val} | Tail], ProtoMsg) ->
     set(Tail, ProtoMsg#message{octets = Val}).

