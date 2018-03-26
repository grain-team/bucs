-module(bucuri).

-export([
         join/2
         , join/1
         , type/1
        ]).

-type uri_type() :: http | https | ftp | ssh | sftp.

% @doc
% Joins two URI paths with URI separator.
% @end
-spec join(URI1 :: string() | binary(), URI2 :: string() | binary()) -> string() | binary().
join(A, B) ->
  join([A, B]).

% @doc
% Joins a list of URI paths with URI separator.
% @end
-spec join(URIs :: [string() | binary()]) -> string() | binary().
join(URIs) when is_list(URIs) ->
  Join = join(URIs, [], true),
  case {is_list(Join), start_with_sep(URIs)} of
    {true, true} ->
      "/" ++ Join;
    {false, true} ->
      <<"/", Join/binary>>;
    _ ->
      Join
  end.

% @doc
% Return the <tt>URI</tt> type (scheme)
% @end
-spec type(URI :: string() | binary()) -> {ok, uri_type()} | error.
type(URI) when is_list(URI); is_binary(URI) ->
  case http_uri:parse(URI) of
    {error, _} -> error;
    {ok, {Type, _, _, _, _, _}} -> {ok, Type}
  end.

join([], Acc, String) ->
  URI = string:join(Acc, "/"),
  if
    String == true -> URI;
    true -> list_to_binary(URI)
  end;
join([C|Rest], Acc, _) when is_binary(C) ->
  join([bucs:to_list(C)|Rest], Acc, false);
join([C], Acc, String) when is_list(C) ->
  join([], Acc ++ [string:strip(C, left, $/)], String);
join([C|Rest], Acc, String) when is_list(C) ->
  join(Rest, Acc ++ [string:strip(C, both, $/)], String).

start_with_sep([[$/|_]|_]) -> true;
start_with_sep([<<"/", _/binary>>|_]) -> true;
start_with_sep(_) -> false.
