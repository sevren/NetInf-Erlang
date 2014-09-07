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
%%%  This module defines the behaviour of the database processes. 
%%% @end
%%% Created : 7 Nov 2012 by Kiril Goguev
%%%-------------------------------------------------------------------
-module(nn_database).

-export([behaviour_info/1]).

%%--------------------------------------------------------------------
%% @doc
%% Callback modules 
%% init/0, publish/1,  get/1, unpublish/1, search/1, truncate/0 
%% @end
%%--------------------------------------------------------------------

behaviour_info(callbacks) -> [{init, 0}, 
			      {publish, 1}, 
			      {get, 1},
			      {unpublish, 1}, 
			      {search, 1}, 
			      {flush, 0}
			     ];
behaviour_info(_) -> undefined. 

