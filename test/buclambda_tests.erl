-module(buclambda_tests).

-include_lib("eunit/include/eunit.hrl").

buclambda_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        {ok, F} = buclambda:curry(lists, keyfind, 3, [toto, 1]),
        ?assertMatch({toto, 2}, F([{tata, 1}, {toto, 2}, {titi, 3}])),
        ?assertMatch(false, F([{tata, 1}, {tutu, 2}, {titi, 3}]))
    end,
    fun() ->
        {ok, F} = buclambda:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]),
        ?assertMatch({toto, 2}, F(toto)),
        ?assertMatch(false, F(tutu))
    % TODO: end,
    % TODO: fun() ->
    % TODO:     {ok, F} = buclambda:curry(fun(A, B, C) -> A * (B + C) end, [3, 7]),
    % TODO:     ?assertEqual(24, F(1)),
    % TODO:     ?assertEqual(27, F(2)),
    % TODO:     ?assertEqual(30, F(3))
    % TODO: end,
    % TODO: fun() ->
    % TODO:     {ok, F} = buclambda:rcurry(fun(A, B, C) -> A * (B + C) end, [3, 7]),
    % TODO:     ?assertEqual(10, F(1)),
    % TODO:     ?assertEqual(20, F(2)),
    % TODO:     ?assertEqual(30, F(3))
    end,
    fun() ->
        {ok, F} = buclambda:pipe(
                    [
                     buclambda:f_curry(fun lists:filter/2, [fun(E) -> E > 10 end]),
                     fun erlang:length/1
                    ]
                   ),
        ?assertEqual(0, F([1, 2, 3, 4, 5])),
        ?assertEqual(2, F([8, 9, 10, 11, 12]))
    end,
    fun() ->
        {ok, F} = buclambda:compose(
                    [
                     fun erlang:length/1,
                     buclambda:f_curry(fun lists:filter/2, [fun(E) -> E > 10 end])
                    ]
                   ),
        ?assertEqual(0, F([1, 2, 3, 4, 5])),
        ?assertEqual(2, F([8, 9, 10, 11, 12]))
    end
   ]}.
