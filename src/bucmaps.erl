-module(bucmaps).

-export([from_list/1, from_list/2, to_list/1, to_list/2]).

% @doc
% Takes a list of key-value tuples elements and builds a map ; in deep.
% @end
-spec from_list(List :: list()) -> map().
from_list(List) when is_list(List) ->
  from_list(List, all).

% @doc
% Takes a list of key-value tuples elements and builds a map ;
% with the given deep.
% @end
-spec from_list(List :: list(), Deep :: integer() | all) -> map().
from_list(List, Deep) when is_list(List),
                           (is_integer(Deep) orelse Deep =:= all) ->
  from_list(List, Deep, 1).

% @doc
% Returns a list of pairs representing the key-value associations of Map ; in deep
% @end
-spec to_list(Map :: map()) -> list().
to_list(Map) when is_map(Map) ->
  to_list(Map, all).

% @doc
% Returns a list of pairs representing the key-value associations of Map ;
% with the given deep.
% @end
-spec to_list(Map :: map(), Deep :: integer() | all) -> list().
to_list(Map, Deep) when is_map(Map),
                        (is_integer(Deep) orelse Deep =:= all) ->
  to_list(Map, Deep, 1).

from_list(List, Deep, Level) ->
  Map = maps:from_list(List),
  if
    Level < Deep orelse Deep =:= all ->
      term_to_map(Map, Deep, Level + 1);
    true ->
      Map
  end.

term_to_map(Term, Deep, Level) when is_tuple(Term) ->
  list_to_tuple(
    [term_to_map(Element, Deep, Level) || Element <- tuple_to_list(Term)]
   );
term_to_map(List, Deep, Level) when is_list(List) ->
  case bucs:is_kw_list(List) of
    true ->
      from_list(List, Deep, Level);
    false ->
      [term_to_map(Element, Deep, Level) || Element <- List]
  end;
term_to_map(Map, Deep, Level) when is_map(Map) ->
  maps:map(fun(_, V) ->
               case bucs:is_kw_list(V) of
                 true ->
                   from_list(V, Deep, Level + 1);
                 _ ->
                   term_to_map(V, Deep, Level + 1)
               end
           end, Map);
term_to_map(Term, _Deep, _Level) ->
  Term.


to_list(Map, Deep, Level) ->
  List = maps:to_list(Map),
  if
    Level < Deep orelse Deep =:= all ->
      lists:map(fun
                  ({K, V}) when is_map(V) ->
                    {K, to_list(V, Deep, Level + 1)};
                  ({K, V}) when is_tuple(V) ->
                    {K, list_to_tuple(term_to_list(tuple_to_list(V), Deep, Level))};
                  ({K, V}) when is_list(V) ->
                    {K, term_to_list(V, Deep, Level)};
                  (E) ->
                    E
                end, List);
    true ->
      List
  end.

term_to_list(List, Deep, Level) ->
  [case E of
     E when is_map(E) ->
       to_list(E, Deep, Level + 1);
     E when is_list(E) ->
       term_to_list(E, Deep, Level + 1);
     E when is_tuple(E) ->
       list_to_tuple(term_to_list(tuple_to_list(E), Deep, Level + 1));
     _ ->
       E
   end || E <- List].
