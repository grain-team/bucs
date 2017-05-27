-module(bucfloat).

-export([round/2]).

round(Number, Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.
