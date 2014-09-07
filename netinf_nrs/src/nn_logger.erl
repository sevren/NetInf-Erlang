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
%%% @author Kiril Goguev
%%% @author Daniele Bacarella
%%% @author Marcus Ihlar
%%% @doc
%%%  This module implements the log function 
%%% @end
%%% Created : 19 Oct 2012 by Kiril Goguev and Daniele Bacarella
%%%-------------------------------------------------------------------
-module(nn_logger).

-include_lib("eunit/include/eunit.hrl").

%external functions
-export([
	 log/4,
	 log/1
	]).

-define(LOG_FILE,"logs/log.txt").
-define(SOURCE,"logs/log.txt").
-define(LOG_EXT,".txt").

%This value is in BYTES!
-define(LOG_FILE_SIZE, 1024000).

%%%==========================================================================
%% API functions
%%%==========================================================================

%%--------------------------------------------------------------------
%% @doc
%% logs whatever is sent in the function arguments 
%% @end
%%--------------------------------------------------------------------
-spec log(atom(), atom(), atom(), list()) -> {ok, pid()}.
log(Level, Module, Action, Msg)->
    check_file_size(),
    {ok, FileId} = create_file(),
    log(FileId, Level, Module, Action, Msg),
    close_file(FileId),
    {ok, FileId}.

log(Logs) ->
    check_file_size(),
    {ok, FileId} = create_file(),
    log(lists:reverse(Logs), FileId),
    close_file(FileId).

log([], _) ->
    ok;
log([Log | Logs], FileId) ->
    io:fwrite(FileId,"~p : ~p ~p ~p ~p~n", Log),
    log(Logs, FileId).

%%%==========================================================================
%%% Internal Functions
%%%==========================================================================

%% @doc 
%%  creates the log file 
%%  Returns {ok,FileId} where FileId is the file descripter handle
%% @end
create_file() ->
    {ok, Fileid} = file:open(?LOG_FILE, [read, write, append]),
    {ok, Fileid}.


%% @doc Shows what message has been logged and send it to a file
%%  PARAMETERS:
%%  - FileId: the file handle for the current log file.
%%  - Level: verbose
%%  - Module: the name of the module in which this function is called
%%  - Action: the name of the function in which this log function is called 
%%  - Msg:    the actual log message
%% @end
log(FileId, Level, Module, Action, Msg) ->
    Ts = binary_to_list(nn_util:create_timestamp()),
    io:fwrite(FileId,"~p : ~p ~p ~p ~p~n",
	      [Ts, Level, Module, Action, Msg]).

%% @doc 
%%  Closes the File in order to flush everything written to it
%%  Parameters: 
%%  - FileId: the file handle for the current log file
%% @end 
close_file(FileId)->
    file:close(FileId).

%% @doc 
%%  checks the size of the current log file to see if we have to 
%%  move it and create a new one.
%% @end
check_file_size()->
    case filelib:file_size(?LOG_FILE) > ?LOG_FILE_SIZE of
	true ->
	    Ts = binary_to_list(nn_util:create_timestamp()),
	    Destination = lists:append(["logs/old/log-", 
					Ts ,
					?LOG_EXT]),
	    file:rename(?SOURCE, Destination);
	false  -> 
	    ok
    end.
	    
