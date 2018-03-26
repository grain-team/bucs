-module(bucfloat).

-export([round/2]).

% @doc round <tt>Number</tt> with the given <tt>Precision</tt>.
-spec round(Number :: float(), Precision :: integer()) -> float().
round(Number, Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.
