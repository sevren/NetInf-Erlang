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
%%% @author Jon Borglund
%%% @doc
%%% Manages Stats on current NetInf Nodes. 
%%% @end
%%% Created : 28 Nov 2012 by Thomas Nordström
%%%-------------------------------------------------------------------
-module(nn_stats).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
	 fetch/0,
	 update/2,
	 add_to_list/2,
	 remove_from_list/2
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  msgid_list = [] :: list(),
	  active_count = 0 :: integer(),
	  request_count = 0 :: integer(),
	  forwarded_reqs_count = 0 :: integer(),
	  forwarded_times_count = 0 :: integer(),
	  tot_hit_count = 0 :: integer(),
	  local_hit_count = 0 :: integer(),
	  not_found_count = 0 :: integer(),
	  ndo_count = 0 :: integer(),
	  octet_count = 0 :: integer()
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%  Get all the stats of the server
%% @end
%%--------------------------------------------------------------------
-spec fetch() -> json:json_object().
fetch() ->
    gen_server:call(?SERVER, fetch).

%%--------------------------------------------------------------------
%% @doc
%%  Update a field with the specified value
%% @end
%%--------------------------------------------------------------------
-spec update(atom(), integer()) -> ok.
update(What, Value) ->
    gen_server:cast(?SERVER, {update, What, Value}).

%%--------------------------------------------------------------------
%% @doc
%%  Add the value to the specified list if it does not already exist
%% @end
%%--------------------------------------------------------------------
-spec add_to_list(atom(), term()) -> ok.
add_to_list(What, Value) ->
    gen_server:cast(?SERVER, {add_to_list, What, Value}).

%%--------------------------------------------------------------------
%% @doc
%%  Removes the specified value from the list if it exists
%% @end
%%--------------------------------------------------------------------
-spec remove_from_list(atom(), term()) -> ok.
remove_from_list(What, Value) ->
    gen_server:cast(?SERVER, {remove_from_list, What, Value}).

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
init([]) ->
    {ok, #state{}}.

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
handle_call(fetch, _, State = #state{
      msgid_list = MsgIdList,
      active_count = ActiveCount,
      request_count = RequestCount,
      forwarded_reqs_count = ForwardedReqsCount,
      forwarded_times_count = ForwardedTimesCount,
      tot_hit_count = TotHitCount,
      local_hit_count = LocalHitCount,
      not_found_count = NotFoundCount,
      ndo_count = NdoCount,
      octet_count = OctetCount}) ->
    Reply = {[
	      {msgid_list, MsgIdList},
	      {active_count, ActiveCount},
	      {request_count, RequestCount},
	      {forwarded_reqs_count, ForwardedReqsCount},
	      {forwarded_times_count, ForwardedTimesCount},
	      {tot_hit_count, TotHitCount},
	      {local_hit_count, LocalHitCount},
	      {not_found_count, NotFoundCount},
	      {ndo_count, NdoCount},
	      {octet_count, OctetCount}
	     ]},
    {reply, {ok, Reply}, State};
handle_call(_,_,State) ->
    {reply, ok, State}.

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
handle_cast({update, What, Value}, State) ->
    handle_update(What, Value, State);
handle_cast({add_to_list, What, Value}, State) ->
    handle_add_to_list(What, Value, State);
handle_cast({remove_from_list, What, Value}, State) ->
    handle_remove_from_list(What, Value, State);
handle_cast(_, State) ->
    {noreply, State}.

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
handle_info(_, State) ->
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
terminate(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_, State, _) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_remove_from_list(msgid_list, Value, State) ->
    case is_list(Value) of
	true ->
	    NewValue = lists:foldl(
			 fun(X, Acc) -> 
				 lists:delete(X, Acc)
			 end,
			 State#state.msgid_list,
			 Value);
	_ ->
	    NewValue = lists:delete(Value, State#state.msgid_list)
    end,
    {noreply, State#state{msgid_list = NewValue}}.

handle_add_to_list(msgid_list, Value, State) ->
    case is_list(Value) of
	true ->
	    NiceValue = lists:usort(Value);
	_ ->
	    NiceValue = [Value]
    end,
    NewValue = lists:umerge(State#state.msgid_list, NiceValue),
    {noreply, State#state{msgid_list = NewValue}}.



handle_update(active_count, Value, State) ->
    NewValue = State#state.active_count + Value,
    {noreply, State#state{active_count = NewValue}};
handle_update(request_count, Value, State) ->
    NewValue = State#state.request_count + Value,
    {noreply, State#state{request_count = NewValue}};
handle_update(forwarded_times_count, Value, State) ->
    NewValue = State#state.forwarded_times_count + Value,
    {noreply, State#state{forwarded_times_count = NewValue}};
handle_update(forwarded_reqs_count, Value, State) ->
    NewValue = State#state.forwarded_reqs_count + Value,
    {noreply, State#state{forwarded_reqs_count = NewValue}};
handle_update(tot_hit_count, Value, State) ->
    NewValue = State#state.tot_hit_count + Value,
    {noreply, State#state{tot_hit_count = NewValue}};
handle_update(local_hit_count, Value, State) ->
    NewValue = State#state.local_hit_count + Value,
    {noreply, State#state{local_hit_count = NewValue}};
handle_update(not_found_count, Value, State) ->
    NewValue = State#state.not_found_count + Value,
    {noreply, State#state{not_found_count = NewValue}};
handle_update(ndo_count, Value, State) ->
    NewValue = State#state.ndo_count + Value,
    {noreply, State#state{ndo_count = NewValue}};
handle_update(octet_count, Value, State) ->
    NewValue = State#state.octet_count + Value,
    {noreply, State#state{octet_count = NewValue}}.
