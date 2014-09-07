%%%------------------------------------------------------------------
%%% @author Jon "Don Jon" Borglund
%%% @author Daniele "Don Juan" Bacarella
%%% @copyright (C) 2012, ERNI Group
%%% @doc
%%%  Module used for evaluation of the system
%%% @end
%%% Created :  14 Jan 2013 by ERNIS
%%%-------------------------------------------------------------------
-module(nn_evaluation).

-export([
	 n_pub/1,
	 mod_n_pub/1
	]).

-include_lib("eunit/include/eunit.hrl").


%-define(BASEURL, "http://130.238.15.220:9999/netinfproto").
-define(BASEURL, "http://130.238.15.225:9999/netinfproto").
-define(NRSURL, "http://130.238.15.220:9999/netinfproto").
-define(GETURL, ?BASEURL "/get").
-define(PUBLISH_LOCAL_URL, ?BASEURL "/publish").
-define(SEARCHURL, ?BASEURL  "/search").
-define(PUBLISH_NRS_URL, ?NRSURL "/publish").

% Used to publish a pure test stream with N chunks
n_pub(N) ->
    Bin = nn_util:n_byte_binary(100),
    n_pub(N, 1, binary:copy(Bin, 10000)).
n_pub(N, N, _) ->
    ok;
n_pub(N, M, Bin) ->
    Bin1 = binary:encode_unsigned(M),
    %?debugVal(Bin1),
    Binary = <<Bin/binary, Bin1/binary>>,
    Name = "ni:///sha-256;" ++ hash(Binary),
    % Local publish
    _Ret = generate_publish([ 
		       {'URI', Name}, 
		       {msgid, unique_id()},
		       {loc, ?BASEURL},
		       {fullPut, "true"},
		       {ext, 
			"{\"meta\": \"mystream\", \"chunk\": \"mystream" ++ 
			    integer_to_list(M) ++ "\"}"
		       }		       
		     ], [{octets, Binary}], ?PUBLISH_LOCAL_URL), 
    
    % To the central NRS
    Ret1 = generate_publish([ 
		       {'URI', Name}, 
		       {msgid, unique_id()},
		       {loc, ?BASEURL},
		       {fullPut, "false"},
		       {ext, 
			"{\"meta\": \"mystream\", \"chunk\": \"mystream" ++ 
			    integer_to_list(M) ++ "\"}"
		       }		       
		     ], [{octets, <<>>}], ?PUBLISH_NRS_URL),
    ?debugVal(Ret1),
    n_pub(N, M + 1, Bin).


% Used for publish a modified stream with N chunks
mod_n_pub(N) ->
    StreamName = "abc",
    Name = "ni:///sha-256;" ++ StreamName,
    _Ret = generate_publish([ 
			      {'URI', Name}, 
			      {msgid, unique_id()},
			      {loc, ?BASEURL},
			      {fullPut, "false"},
			      {ext, 
			       "{\"meta\": \"this is a chunked stream\"}"
			      }		       
			    ], [{octets, <<>>}], ?PUBLISH_LOCAL_URL),    
    Bin = nn_util:n_byte_binary(100),
    mod_n_pub(N, 1, binary:copy(Bin, 10000), Name).

mod_n_pub(N, N, _, _) ->
    ok;
mod_n_pub(N, M, Bin, Name) ->
   {ok, _} = store(Name ++ integer_to_list(M), Bin),
   mod_n_pub(N, M + 1, Bin, Name). 
  

store(Name, Octets) ->
    CH = nn_content_handler:spawn(),
    nn_content_handler:store_content_without_validating(CH, Name, Octets),
    nn_content_handler:kill(CH),
    {ok, stored}.


unique_id() ->
    {_, _, Micros} = now(),
    integer_to_list(Micros).
 
%truncate_hash(HashedBinary, TruncationNumber)->
%    <<TruncHash:TruncationNumber, _/binary >> = HashedBinary,
%    <<TruncHash:TruncationNumber>>.

hash(Binary) ->
    strip(
      base64:encode(
	crypto:hash(sha256, Binary)
       )
     ).

strip(Hash) ->
    string:strip(binary_to_list(Hash), both, $=).

generate_publish(Publish, Data, PUBURL) ->
    %inets:start(),

    Boundary = "------WebKitFormBoundaryjTwy4nYi2Aj6shCW",
    Body = format_multipart_formdata(
	     Boundary, Publish, Data),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(byte_size(Body))}],
    
    case httpc:request(post, {PUBURL, Headers, ContentType, Body}, 
		       [], [{body_format, binary}]) of
	{ok, {_, [{_, <<"multipart/form-data;", Rest/binary>>}], ResponseBody}} ->
	    %inets:stop(),
	    nn_util:decode(Rest, ResponseBody);
	{ok, {_, HList, ResponseBody}} ->
	    %inets:stop(),
	    HList ++ nn_util:decode(ResponseBody)
    end.

format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(
		   fun({FieldName, FieldContent}) ->
			  format_one_part(FieldName, FieldContent, Boundary)
		   end, Fields),
    FieldParts2 = list_to_binary(FieldParts),

    FileParts = lists:map(
		  fun({FieldName, FileContent}) ->
			  format_one_part(FieldName, FileContent, Boundary)
		  end, Files),
    FileParts2 = list_to_binary(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    list_to_binary([
		    FieldParts2, FileParts2, EndingParts
		   ]).

format_one_part(octets, Binary, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(octets), "\""]),
     lists:concat([
		   "Content-Type: ", "application/octet-stream"]),
     "\r\n\r\n",
     Binary, "\r\n"];
format_one_part(FieldName, FieldContent, Boundary) ->
    [lists:concat(["--", Boundary, "\r\n"]),
     lists:concat(["Content-Disposition: form-data; name=\"",
		   atom_to_list(FieldName), "\""
		  ]),
     "\r\n\r\n",
     FieldContent, "\r\n"].
