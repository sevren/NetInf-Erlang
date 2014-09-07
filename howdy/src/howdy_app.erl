-module(howdy_app).
-behaviour(application).


%% Application callbacks
-export([start/2, stop/1, onresponse_hook/4, onrequest_hook/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

onresponse_hook(Status, Headers, IO, Req) ->
  case Status of
    404 ->
      %% io:format("Strange message~n~p~n~p~n~p~n",[Headers, IO, Req]),
      Req;
    _ ->
      %% io:format("Ordinary message~n",[]),
      Req
  end.

onrequest_hook(Req) ->
    ok.



start(_StartType, _StartArgs) ->
    Dispatch = [
		{'_', [
		       %{[], howdy_http_handler, []},
		       {'_', howdy_http_handler,[]}
		      ]}
	       ],
    {ok, _} = cowboy:start_http(http, 100, [{port, 1055}], 
				[{dispatch, Dispatch}]),
				 %% ,{onresponse, fun onresponse_hook/4},
				 %% {onrequest, fun onrequest_hook/1}]),
    howdy_sup:start_link().

stop(_State) ->
    ok.
