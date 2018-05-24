-module(bucbinary).

-export([
         join/2
         , trim/2
         , is_integer/1
         , is_float/1
         , are_integers/1
         , are_floats/1
         , to_hex/1
         , to_hex/2
         , from_hex/1
         , rand_bits/1
        ]).

% Deprecated
-export([
         to_hexstr/1
         , from_hexstr/1
        ]).

%% @doc
%% Generate random binary
%% @end
-spec rand_bits(Bits :: non_neg_integer()) -> binary().
rand_bits(Bits) when erlang:is_integer(Bits), Bits >= 0->
  Bytes = (Bits + 7) div 8,
  <<Result:Bits/bits, _/bits>> = crypto:strong_rand_bytes(Bytes),
  Result.

%% @deprecated use bucbinary:to_hex/1
-spec to_hexstr(Bin :: binary()) -> list().
to_hexstr(Bin) when is_binary(Bin) ->
  to_hex(Bin).

%% @doc
%% Convert a binary to and Hex string
%% @end
-spec to_hex(Bin :: binary()) -> string().
to_hex(Bin) when is_binary(Bin) ->
  to_hex(Bin, string).

%% @doc
%% Convert a binary to and Hex string or binary
%% @end
-spec to_hex(Bin :: binary(), Output :: string | binary) -> string() | binary().
to_hex(Bin, string) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]);
to_hex(Bin, binary) ->
  bucs:to_binary(to_hex(Bin, string)).

%% @deprecated use bucbinary:from_hex/1
-spec from_hexstr(String :: list()) -> binary().
from_hexstr(S) when is_list(S) ->
  hexstr_to_bin(S, []).

%% @doc
%% Convert aa Hex string or binary to binary
%% @end
-spec from_hex(S :: string() | binary()) -> binary().
from_hex(S) when is_list(S) ->
  hexstr_to_bin(S, []);
from_hex(S) when is_binary(S) ->
  from_hex(bucs:to_list(S)).

hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hexstr_to_bin(T, [V | Acc]).

%% @doc
%% join a list of binaries with the given separator
%%
%% Example:
%%
%% <pre lang="erlang">
%% &lt;&lt;"toto-tata-titi"&gt;&gt; = bucbinary:join([&lt;&lt;"toto"&gt;&gt;, &lt;&lt;"tata"&gt;&gt;, &lt;&lt;"titi"&gt;&gt;], &lt;&lt;"-"&gt;&gt;).
%% </pre>
%% @end
-spec join([term()], binary()) -> binary().
join([], _) ->
  <<>>;
join(L, S) when is_list(L), is_binary(S) ->
  join(L, S, <<>>);
join(B, _) when is_binary(B) ->
  B.
join([], _, Acc) ->
  Acc;
join([E|R], S, <<>>) ->
  join(R, S, bucs:to_binary(E));
join([E|R], S, Acc) ->
  join(R, S, <<Acc/binary, S/binary, (bucs:to_binary(E))/binary>>).

% @doc
% Trim a binary
% @end
- spec trim(Binary :: binary(), Direction :: left | right | both) -> binary().
trim(Binary, left) ->
  trim_left(Binary);
trim(Binary, right) ->
  trim_right(Binary);
trim(Binary, both) ->
  trim_left(trim_right(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s orelse
                                   C =:= $\n orelse
                                   C =:= $\r orelse
                                   C =:= $\t ->
  trim_left(Rest);
trim_left(Binary) -> Binary.

trim_right(Binary) ->
  trim_right(Binary, size(Binary)-1).

trim_right(Binary, Size) ->
  case Binary of
    <<Rest:Size/binary, C>> when C =:= $\s
                                 orelse C =:= $\t
                                 orelse C =:= $\n
                                 orelse C =:= $\r ->
      trim_right(Rest, Size - 1);
    Other ->
      Other
  end.

% @doc
% Return true if all binaries in the given list can be converted to float ; false otherwise
% @end
-spec are_floats(Binaries :: [binary()]) -> true | false.
are_floats([]) ->
  false;
are_floats(List) ->
  are_floats(List, true).
are_floats([], Acc) ->
  Acc;
are_floats([E|Rest], Acc) ->
  are_floats(Rest, bucbinary:is_float(E) andalso Acc).

% @doc
% Return true if the given binary can be converted to float ; false otherwise
% @end
-spec is_float(Binary :: binary()) -> true | false.
is_float(<<>>) ->
  false;
is_float(Data) ->
  case binary:split(Data, [<<".">>]) of
    [_, _] = V ->
      are_integers(V);
    _ ->
      false
  end.

% @doc
% Return true if all binaries in the given list can be converted to integer ; false otherwise
% @end
-spec are_integers(Binaries :: [binary()]) -> true | false.
are_integers([]) ->
  false;
are_integers(List) ->
  are_integers(List, true).
are_integers([], Acc) ->
  Acc;
are_integers([E|Rest], Acc) ->
  are_integers(Rest, bucbinary:is_integer(E) andalso Acc).

% @doc
% Return true if the given binary can be converted to integer ; false otherwise
% @end
-spec is_integer(Binary :: binary()) -> true | false.
is_integer(<<>>) ->
  false;
is_integer(Data) ->
  do_is_integer(Data).


do_is_integer(<<>>) ->
  true;
do_is_integer(<<"0", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"1", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"2", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"3", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"4", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"5", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"6", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"7", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"8", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"9", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(_) ->
  false.
