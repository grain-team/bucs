-module(bucfloat_tests).
-include_lib("eunit/include/eunit.hrl").

bucfloat_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assertEqual(1.23, bucfloat:round(1.2311111, 2)),
        ?assertEqual(1.24, bucfloat:round(1.2355555, 2))
    end
   ]}.
