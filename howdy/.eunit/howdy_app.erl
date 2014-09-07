-module(howdy_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
		{'_', [
		       {[], howdy_http_handler, []}
		      ]}
	       ],
    {ok, _} = cowboy:start_http(http, 100, [{port, 1055}], 
				[{dispatch, Dispatch}]),
    howdy_sup:start_link().

stop(_State) ->
    ok.
