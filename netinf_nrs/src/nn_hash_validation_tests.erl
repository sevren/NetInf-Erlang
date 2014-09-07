%%%-------------------------------------------------------------------
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

%%% @author Alex Lindholm
%%% @author Kiril Goguev
%%% @doc
%%%  This module provides eunit test functions to validate the
%%%  hash_validation module.
%%% @end
%%% Created : 8 Nov 2012 by Kiril Goguev
%%%-------------------------------------------------------------------
-module(nn_hash_validation_tests).

-include_lib("eunit/include/eunit.hrl").

validation_test_()->
    File=list_to_binary("nn_app.erl"),
    Name =  list_to_binary(
	      "ni:///sha-256;Jvs4OKqngn6KobGsd5EMZ94fy4ckpOByUGrJXJaEylw"),
    Name2 = list_to_binary(
	      "ni:///sha-256;Jvs4OKqngn6KobGsd5EMZ94f48kp9OByUGrJXJaEylw"), 
    ActualHash= list_to_binary(
		  "Jvs4OKqngn6KobGsd5EMZ94fy4ckpOByUGrJXJaEylw"),
    OutPut=nn_hash_validation:validate(Name,File),
    OutPut2=nn_hash_validation:validate(Name2,File),

    Hash = crypto:hash(sha256, File),
    TruncatedHash64 = <<38, 251, 56, 56, 170, 167, 130, 126>>,
    TruncatedHash128 = <<38, 251, 56, 56, 170, 167, 130, 126, 138, 161,
			 177, 172, 119, 145, 12, 103>>,
    OutPut64 = nn_truncate_hash:truncate(Hash, 64),
    OutPut128 = nn_truncate_hash:truncate(Hash, 128),

    [
     ?_assertEqual({ok, valid, ActualHash}, OutPut),
     ?_assertEqual({ok,invalid}, OutPut2),
     ?_assertEqual(TruncatedHash64, OutPut64),
     ?_assertEqual(TruncatedHash128, OutPut128)    
    ].
