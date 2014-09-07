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
%%% @author Alex Lindholm
%%% @author Kiril Goguev
%%% @doc
%%%  This module provides functions to validate the NDO content.
%%%  Currently supported hash algorithms are SHA256-64. Where 64 is the base
%%%  used for content validation
%%% @end
%%% Created : 8 Nov 2012 by Kiril Goguev
%%%-------------------------------------------------------------------

-module(nn_hash_validation).
-export([validate/2, parse_name/1]).

%%%=====================================================================
%%% API
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc 
%%  Takes a NetInf Name and performs the validation of the hash. 
%%  Function returns wether or not the content hash is valid.
%% @end
%%----------------------------------------------------------------------
-spec validate(Name :: binary(), File :: binary()) ->
		      {ok, valid, binary()} | {ok, invalid}.

validate(Name, File) ->
    {HashAlgo, ExpectedHash} = parse_name(Name),
    ActualHash = strip(get_hash_of_file(HashAlgo, File)),
    nn_logger_server:log(verbose, ?MODULE, validate,
			 ["Actual hash: ", ActualHash, 
			  "Expected hash: ", ExpectedHash, 
			  "HashAlgo: ", HashAlgo]),
    case ExpectedHash =:= ActualHash of 
	true ->
	    {ok, valid, ActualHash};
	false ->
	    {ok, invalid, ActualHash}
    end.

%%----------------------------------------------------------------------
%% @doc 
%%  Parses an ni name and extracts the hash value and algorithm.
%% @end
%%----------------------------------------------------------------------
-spec parse_name(string()) -> {string(), string()}.
parse_name(Name) ->
   {match, [Algo, ExpectedHash]} = re:run(Name, "^ni://.*/([^;]+);(.+)$",
		       [{capture, [1, 2], binary}]),
    {Algo, ExpectedHash}.

%%%=====================================================================
%%% Internal functions 
%%%=====================================================================
strip(Hash) ->
    list_to_binary(string:strip(binary_to_list(Hash), both, $=)).
    
get_hash_of_file(HashAlgo,File)->
    case HashAlgo of
	<<"sha-256">> ->
	    sha_256(File);
	<<"sha-256-64">> ->
	    sha_256_truncated(File, 64);
	<<"sha-256-128">> ->
	    sha_256_truncated(File, 128);
	_ ->
	    undefined
    end.

sha_256(FileContents)->
    HashedValue=crypto:hash(sha256, FileContents),
    base64:encode(HashedValue).


sha_256_truncated(FileContent, TruncationNumber)->
    HashValue=crypto:hash(sha256, FileContent),
    TruncatedHash=truncate(HashValue, TruncationNumber),
    base64:encode(TruncatedHash).

truncate(HashedBinary,TruncationNumber)->
   <<TruncHash:TruncationNumber, _/binary>> = HashedBinary,
   <<TruncHash:TruncationNumber>>.
