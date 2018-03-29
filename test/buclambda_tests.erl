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
    end
   ]}.
