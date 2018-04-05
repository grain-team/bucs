-module(buclambda).

-export([
         pipe/1
         , compose/1

         , curry/1
         , curry/2
         , curry/3
         , curry/4

         , rcurry/1
         , rcurry/2
         , rcurry/3
         , rcurry/4
        ]).

% @doc
% Performs left-to-right function composition.
% @end
-spec pipe(Functions :: [function() | {function(), [term()]}]) -> {ok, function()} | error.
pipe(_Functions) ->
  todo.

% @doc
% Performs right-to-left function composition.
% @end
-spec compose(Functions :: [function()]) -> {ok, function()} | error.
compose(_Functions) ->
  todo.

% @doc
% Returns a curried equivalent of the provided function.
%
% <pre lang="erlang">
% curry:curry(fun lists:keystore/4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A0) -&gt; fun(A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
% </pre>
% @end
-spec curry(Fun :: function()) -> {ok, function()} | error.
curry(Fun) when is_function(Fun) ->
  case get_fun_info(Fun) of
    {Module, Function, Arity} ->
      curry(Module, Function, Arity);
    {_Fun, _Arity} ->
      error
      % TODO: currify(Fun, Arity, Arity, lr)
  end.

% @doc
% Returns a curried equivalent of the provided function, with the specified deep or params.
%
% <pre lang="erlang">
% curry:curry(fun lists:keystore/4, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A0) -&gt; fun(A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
%
% curry:curry(fun lists:keystore/4, 3).
% % =&gt; {ok, #Fun&lt;erl_eval.12.99386804&gt;}
% % =&gt; fun(A0, A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end.
%
% curry:curry(fun lists:keystore/4, 2).
% % =&gt;{ok, #Fun&lt;erl_eval.18.99386804&gt;}
% % =&gt; fun(A0, A1, A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end.
%
% curry:curry(fun lists:keystore/4, 1).
% % =&gt; {ok, #Fun&lt;erl_eval.4.99386804&gt;}
% % =&gt; fun(A0, A1, A2, A3) -&gt; lists:keystore(A0, A1, A2, A3) end.
%
% {ok, F} = curry:curry(fun lists:keyfind/3, [toto, 1]).
% F([{tata, 1}, {toto, 2}, {titi, 3}]).
% % =&gt; {toto, 2}
% F([{tata, 1}, {tutu, 2}, {titi, 3}]).
% % =&gt; false
% </pre>
% @end
-spec curry(Fun :: function(), DeepOrArgs :: integer() | [term()]) -> {ok, function()} | error.
curry(Fun, Deep) when is_integer(Deep) orelse is_list(Deep) ->
  case get_fun_info(Fun) of
    {Module, Function, Arity} ->
      curry(Module, Function, Arity, Deep);
    {_Fun, _Arity} ->
      error
      % TODO: case bucs:type(Deep) of
      % TODO:   integer ->
      % TODO:     currify(Fun, Arity, Deep, lr);
      % TODO:   _ ->
      % TODO:     case currify(Fun, Arity, Arity - length(Deep) + 1, lr) of
      % TODO:       {ok, CurryFun} -> {ok, erlang:apply(CurryFun, Deep)};
      % TODO:       Other -> Other
      % TODO:     end
      % TODO: end
  end.

% @doc
% Returns a <i>reverse</i> curried equivalent of the provided function.
%
% <pre lang="erlang">
% curry:rcurry(fun lists:keystore/4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A3) -&gt; fun(A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
% </pre>
-spec rcurry(Fun :: function()) -> {ok, function()} | error.
rcurry(Fun) when is_function(Fun) ->
  case get_fun_info(Fun) of
    {Module, Function, Arity} ->
      rcurry(Module, Function, Arity);
    {_Fun, _Arity} ->
      error
      % TODO: currify(Fun, Arity, Arity, rl)
  end.

% @doc
% Returns a <i>reverse</i> curried equivalent of the provided function, with the specified deep or params.
%
% <pre lang="erlang">
% curry:rcurry(fun lists:keystore/4, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A3) -&gt; fun(A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
%
% curry:rcurry(fun lists:keystore/4, 3).
% % =&gt; {ok, #Fun&lt;erl_eval.12.99386804&gt;}
% % =&gt; fun(A3, A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end.
%
% curry:rcurry(fun lists:keystore/4, 2).
% % =&gt; {ok, #Fun&lt;erl_eval.18.99386804&gt;}
% % =&gt; fun(A3, A2, A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end.
%
% curry:rcurry(fun lists:keystore/4, 1).
% % =&gt; {ok, #Fun&lt;erl_eval.4.99386804&gt;}
% % =&gt; fun(A3, A2, A1, A0) -&gt; lists:keystore(A0, A1, A2, A3) end.
%
% {ok, F} = curry:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]).
% F(toto).
% % =&gt; {toto, 2}
% F(tutu).
% % =&gt; false
% </pre>
% @end
-spec rcurry(Fun :: function(), DeepOrArgs :: integer() | [term()]) -> {ok, function()} | error.
rcurry(Fun, Deep) when is_integer(Deep) orelse is_list(Deep) ->
  case get_fun_info(Fun) of
    {Module, Function, Arity} ->
      rcurry(Module, Function, Arity, Deep);
    {_Fun, _Arity} ->
      error
      % TODO: case bucs:type(Deep) of
      % TODO:   integer ->
      % TODO:     currify(Fun, Arity, Deep, lr);
      % TODO:   _ ->
      % TODO:     case currify(Fun, Arity, Arity - length(Deep) + 1, rl) of
      % TODO:       {ok, CurryFun} -> {ok, erlang:apply(CurryFun, Deep)};
      % TODO:       Other -> Other
      % TODO:     end
      % TODO: end
  end.

% @doc
% Returns a curried equivalent of the provided function.
%
% <pre lang="erlang">
% curry:curry(lists, keystore, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A0) -&gt; fun(A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
% </pre>
% @end
-spec curry(Module :: module(), Function :: atom(), Arity :: integer()) ->
  {ok, function()} | error.
curry(Module, Function, Arity) ->
  currify({Module, Function}, Arity, Arity, lr).

% @doc
% Returns a curried equivalent of the provided function, with the specified deep or params.
%
% <pre lang="erlang">
% curry:curry(lists, keystore, 4, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A0) -&gt; fun(A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
%
% curry:curry(lists, keystore, 4, 3).
% % =&gt; {ok, #Fun&lt;erl_eval.12.99386804&gt;}
% % =&gt; fun(A0, A1) -&gt; fun(A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end end.
%
% curry:curry(lists, keystore, 4, 2).
% % =&gt;{ok, #Fun&lt;erl_eval.18.99386804&gt;}
% % =&gt; fun(A0, A1, A2) -&gt; fun(A3) -&gt; lists:keystore(A0, A1, A2, A3) end end.
%
% curry:curry(lists, keystore, 4, 1).
% % =&gt; {ok, #Fun&lt;erl_eval.4.99386804&gt;}
% % =&gt; fun(A0, A1, A2, A3) -&gt; lists:keystore(A0, A1, A2, A3) end.
%
% {ok, F} = curry:curry(lists, keyfind, 3, [toto, 1]).
% F([{tata, 1}, {toto, 2}, {titi, 3}]).
% % =&gt; {toto, 2}
% F([{tata, 1}, {tutu, 2}, {titi, 3}]).
% % =&gt; false
% </pre>
% @end
-spec curry(Module :: module(), Function :: atom(), Arity :: integer(), DeepOrArgs :: integer() | [term()]) ->
  {ok, function()} | error.
curry(Module, Function, Arity, Deep) when is_integer(Deep) ->
  currify({Module, Function}, Arity, Deep, lr);
curry(Module, Function, Arity, Args) when is_list(Args) ->
  case curry(Module, Function, Arity, Arity - length(Args) + 1) of
    {ok, Fun} -> {ok, erlang:apply(Fun, Args)};
    Other -> Other
  end.

% @doc
% Returns a <i>reverse</i> curried equivalent of the provided function.
%
% <pre lang="erlang">
% curry:rcurry(lists, keystore, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A3) -&gt; fun(A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
% </pre>
% @end
-spec rcurry(Module :: module(), Function :: atom(), Arity :: integer()) ->
  {ok, function()} | error.
rcurry(Module, Function, Arity) ->
  currify({Module, Function}, Arity, Arity, rl).

% @doc
% Returns a <i>reverse</i> curried equivalent of the provided function, with the specified deep or params.
%
% <pre lang="erlang">
% curry:rcurry(lists, keystore, 4, 4).
% % =&gt; {ok, #Fun&lt;erl_eval.6.99386804&gt;}
% % =&gt; fun(A3) -&gt; fun(A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end end.
%
% curry:rcurry(lists, keystore, 4, 3).
% % =&gt; {ok, #Fun&lt;erl_eval.12.99386804&gt;}
% % =&gt; fun(A3, A2) -&gt; fun(A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end end.
%
% curry:rcurry(lists, keystore, 4, 2).
% % =&gt; {ok, #Fun&lt;erl_eval.18.99386804&gt;}
% % =&gt; fun(A3, A2, A1) -&gt; fun(A0) -&gt; lists:keystore(A0, A1, A2, A3) end end.
%
% curry:rcurry(lists, keystore, 4, 1).
% % =&gt; {ok, #Fun&lt;erl_eval.4.99386804&gt;}
% % =&gt; fun(A3, A2, A1, A0) -&gt; lists:keystore(A0, A1, A2, A3) end.
%
% {ok, F} = curry:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]).
% F(toto).
% % =&gt; {toto, 2}
% F(tutu).
% % =&gt; false
% </pre>
% @end
-spec rcurry(Module :: module(), Function :: atom(), Arity :: integer(), DeepOrArgs :: integer() | [term()]) ->
  {ok, function()} | error.
rcurry(Module, Function, Arity, Deep) when is_integer(Deep) ->
  currify({Module, Function}, Arity, Deep, rl);
rcurry(Module, Function, Arity, Args) when is_list(Args) ->
  case rcurry(Module, Function, Arity, Arity - length(Args) + 1) of
    {ok, Fun} -> {ok, erlang:apply(Fun, Args)};
    Other -> Other
  end.

% -----------------------------------------------------------------------------

currify(Function, Arity, Deep, Orient) when ((is_tuple(Function) andalso
                                              size(Function) == 2 andalso
                                              is_atom(element(1, Function)) andalso
                                              is_atom(element(1, Function)))
                                             orelse is_function(Function, Arity)),
                                            is_integer(Arity),
                                            is_integer(Deep),
                                            Deep =< Arity,
                                            (Orient == lr orelse Orient == rl) ->
  FunStr = lists:flatten([currify(Function, Arity, Deep, Deep, Orient), "."]),
  io:format("% =&gt; ~s~n% =&gt; ", [FunStr]),
  case erl_scan:string(FunStr) of
    {ok, Scanned, _} ->
      case erl_parse:parse_exprs(Scanned) of
        {ok, Parsed} ->
          case erl_eval:exprs(Parsed, []) of
            {value, Fun, _Env} -> {ok, Fun};
            _ -> error
          end;
        _Error ->
          error
      end;
    _Error ->
      error
  end;
currify(_Function, _Arity, _Deep, _Orient) ->
  error.

currify({Module, Function}, Arity, _Deep, 0, _Orient) ->
  io_lib:format("~p:~p(~s)", [Module, Function, string:join(args(0, Arity, 1), ", ")]);

% TODO: currify(Function, Arity, _Deep, 0, _Orient) when is_function(Function) ->
% TODO:   io_lib:format("(~s)(~s)", [fun_to_string(Function), string:join(args(0, Arity, 1), ", ")]);

currify(Function, Arity, Deep, N, lr) when N == Deep ->
  NB = Arity - (Deep - 1),
  io_lib:format("fun(~s) -> ~s end", [string:join(args(0, NB, 1), ", "), currify(Function, Arity, Deep, N - 1, lr)]);
currify(Function, Arity, Deep, N, lr) ->
  io_lib:format("fun(A~p) -> ~s end", [(Deep - N + 1) + (Arity - (Deep + 1)), currify(Function, Arity, Deep, N - 1, lr)]);

currify(Function, Arity, Deep, N, rl) when N == Deep ->
  NB = Arity - (Deep - 1),
  io_lib:format("fun(~s) -> ~s end", [string:join(args(Arity - 1, NB, -1), ", "), currify(Function, Arity, Deep, N - 1, rl)]);
currify(Function, Arity, Deep, N, rl) ->
  io_lib:format("fun(A~p) -> ~s end", [N - 1, currify(Function, Arity, Deep, N - 1, rl)]).

args(_Start, 0, _Add) -> [];
args(Start, N, Add) ->
  [lists:flatten(io_lib:format("A~p", [Start]))|args(Start + Add, N - 1, Add)].

get_fun_info(Fun) ->
  Infos = erlang:fun_info(Fun),
  {module, Module} = lists:keyfind(module, 1, Infos),
  {name, Function} = lists:keyfind(name, 1, Infos),
  {arity, Arity} = lists:keyfind(arity, 1, Infos),
  case lists:member({Function, Arity}, Module:module_info(exports)) of
    true ->
      {Module, Function, Arity};
    false ->
      {Fun, Arity}
  end.

% TODO: fun_to_string(Fun) ->
% TODO:   {env, [{_, _, _, Abs}]} = erlang:fun_info(Fun, env),
% TODO:   lists:flatten(erl_pp:expr({'fun', 1, {clauses, Abs}})).
