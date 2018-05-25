-module(bucmaps_tests).

-include_lib("eunit/include/eunit.hrl").

bucmaps_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch(
           #{a := b, c := d},
           bucmaps:from_list([{a, b}, {c, d}]))
        , ?assertMatch(
           #{a := b, c := #{d := e}},
           bucmaps:from_list([{a, b}, {c, [{d, e}]}]))
        , ?assertMatch(
           #{a := b, c := #{d := #{e := f}}},
           bucmaps:from_list([{a, b}, {c, [{d, [{e, f}]}]}]))
        , ?assertMatch(
           #{a := b, c := #{d := [{e, f}]}},
           bucmaps:from_list([{a, b}, {c, [{d, [{e, f}]}]}], 2))
        , ?assertMatch(
           #{a := 1, b := [c, {d, #{e := 2, f := 3}}]},
           bucmaps:from_list([{a, 1}, {b, [c, {d, [{e, 2}, {f, 3}]}]}]))
        , ?assertMatch(
           #{a := 1, b := #{c := 2, d := #{e := 2, f := 3}}},
           bucmaps:from_list([{a, 1}, {b, #{c => 2, d => [{e, 2}, {f, 3}]}}]))
    end,
    fun() ->
        ?assertEqual(
           [{a, b}, {c, d}],
           bucmaps:to_list(#{a => b, c => d})),
        ?assertEqual(
           [{a, b}, {c, [{d, e}]}],
           bucmaps:to_list(#{a => b, c => #{d => e}})),
        ?assertEqual(
           [{a, b}, {c, [{d, [{e, f}]}]}],
           bucmaps:to_list(#{a => b, c => #{d => #{e => f}}})),
        ?assertEqual(
           [{a, b}, {c, [{d, [{e, f}]}]}],
           bucmaps:to_list(#{a => b, c => #{d => [{e, f}]}}, 2)),
        ?assertEqual(
           [{a, b}, {c, [{d, [[{e, 1}, {f, 2}], [{e, 10}, {f, 20}]]}]}],
           bucmaps:to_list(#{a => b, c => #{d => [#{e => 1, f => 2}, #{e => 10, f => 20}]}})),
        ?assertEqual(
           [{a, b}, {c, [{d, [#{e => 1, f => 2}, #{e => 10, f => 20}]}]}],
           bucmaps:to_list(#{a => b, c => #{d => [#{e => 1, f => 2}, #{e => 10, f => 20}]}}, 2)),
        ?assertEqual(
           [{a, b}, {c, [{d, {[{e, 1}, {f, 2}], [{e, 10}, {f, 20}]}}]}],
           bucmaps:to_list(#{a => b, c => #{d => {#{e => 1, f => 2}, #{e => 10, f => 20}}}})),
        ?assertEqual(
           [{a, b}, {c, [{d, {#{e => 1, f => 2}, #{e => 10, f => 20}}}]}],
           bucmaps:to_list(#{a => b, c => #{d => {#{e => 1, f => 2}, #{e => 10, f => 20}}}}, 2)),
        ?assertEqual(
           [{a, [{b, {c, [e, [{g, 1}, {h, 2}], f], d}}]}],
           bucmaps:to_list(#{a => [{b, {c, [e, #{g => 1, h => 2}, f], d}}]}))
    end
   ]}.
