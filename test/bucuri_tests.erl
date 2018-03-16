-module(bucuri_tests).

-include_lib("eunit/include/eunit.hrl").

bucuri_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_join())
    , ?_test(t_join_with_uri())
    , ?_test(t_type())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_join() ->
  ?assertMatch(<<"/path/to/my/ressource">>,
               bucuri:join([<<"/path">>, "to/my", <<"ressource">>])).

t_join_with_uri() ->
  ?assertMatch(<<"http://example.com/hello/world">>,
               bucuri:join([<<"http://example.com">>, "hello", <<"world">>])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com", "hello", "world"])),
  ?assertMatch("http://example.com/hello/world/",
               bucuri:join(["http://example.com/", "/hello", "/world/"])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com/", "/hello/", "/world"])).

t_type() ->
  ?assertMatch({ok, http}, bucuri:type("http://example.com")),
  ?assertMatch({ok, https}, bucuri:type("https://example.com")),
  ?assertMatch({ok, ftp}, bucuri:type("ftp://example.com")),
  ?assertMatch({ok, ssh}, bucuri:type("ssh://example.com")),
  ?assertMatch({ok, sftp}, bucuri:type("sftp://example.com")),
  ?assertMatch({ok, http}, bucuri:type(<<"http://example.com">>)),
  ?assertMatch({ok, https}, bucuri:type(<<"https://example.com">>)),
  ?assertMatch({ok, ftp}, bucuri:type(<<"ftp://example.com">>)),
  ?assertMatch({ok, ssh}, bucuri:type(<<"ssh://example.com">>)),
  ?assertMatch({ok, sftp}, bucuri:type(<<"sftp://example.com">>)).
