-module(bucos).

-export([
         run/1,
         run/2,
         in/2,
         in/3
        ]).
-define(TIMEOUT, infinity).
-define(DEFAULT_RETURN_TYPE, combined).
-define(CGEXEC, "~ts -g ~ts ~ts").

-type options() :: {timeout, integer()}
| stdout_on_error
| display_stdout
| {return, combined|list, all|last|integer()|[integer()]}
| {cd, string() | binary()}
| {env, [{string(), string() | false}]}.

% @equiv run(Cmd, 5000)
-spec run(string() | binary()) -> term().
run(Cmd) ->
  run(Cmd, ?TIMEOUT).

% @doc
% Execute the given shell command, waiting at most for a given timeout before returning
% <tt>Options</tt> may contain:
% <ul>
% <li><tt>stdout_on_error</tt> : To get standard output in the result, in case of error.</li>
% <li><tt>display_stdout</tt> : Display stdout.</li>
% <li><tt>{timeout, integer() | infinity}</tt> : To set a maximum time to wait for, before returning with a <tt>{error, timeout}</tt> result.</li>
% <li><tt>{return, list|combined, all|last|integer()|[integer()]}</tt> : To specify output collection.</li>
% <li><tt>{cgroup, string() | binary()}</tt> : Run the command in given control groups.</li>
% <li><tt>{cgexec, string() | binary()}</tt> : <tt>cgexec</tt> path.</li>
% <li><tt>{on_data, {function((Data::string(), State::term()) -> NewState::term()), State::term()} | function((Data::string()) -> ok)</tt> :
% Call the given function on each new data with optional metadatas.</li>
% <li><tt>{cd, string() | binary()}</tt> : Change directory before run command.</li>
% <li><tt>{env, [{string(), string() | false}]}</tt> :  The environment of the started process is extended using the environment specifications.</li>
% </ul>
% Note: If more than one shell commands are "chained" in the given string, only the first one is executed.
% @end
-spec run([string() | binary() | {string() | binary(), [term()]}]
          | string()
          | binary()
          | {string() | binary(), [term()]}, integer() | [options()]) ->
  {ok, string()|[string()]} | {error, integer()} | {error, integer(), string()}.
run(Cmd, Timeout) when is_integer(Timeout); Timeout == infinity ->
  run(Cmd, [{timeout, Timeout}]);
run(Cmd, Options) when is_list(Options) ->
  case is_binary(Cmd) orelse bucs:is_string(Cmd) orelse is_tuple(Cmd) of
    true ->
      Timeout = buclists:keyfind(timeout, 1, Options, ?TIMEOUT),
      StdoutOnError = lists:member(stdout_on_error, Options),
      DisplayStdout = lists:member(display_stdout, Options),
      Cgroup = buclists:keyfind(cgroup, 1, Options, undefined),
      Cgexec = buclists:keyfind(cgexec, 1, Options, "cgexec"),
      OnData = buclists:keyfind(on_data, 1, Options, undefined),
      Port = erlang:open_port({spawn, format_command(Cmd, Cgexec, Cgroup)}, run_options(Options, [exit_status, stderr_to_stdout, hide])),
      loop(Port, [], Timeout, StdoutOnError, DisplayStdout, OnData);
    _ ->
      run_all(Cmd, Options, [])
  end;
run(_, _) ->
  error(badarg).

run_options([], Acc) ->
  Acc;
run_options([CD = {cd, _}|Options], Acc) ->
  run_options(Options, [CD|Acc]);
run_options([ENV = {env, _}|Options], Acc) ->
  run_options(Options, [ENV|Acc]);
run_options([_|Options], Acc) ->
  run_options(Options, Acc).

loop(Port, Data, Timeout, StdoutOnError, DisplayStdout, OnData) ->
  receive
    {Port, {data, NewData}} ->
      display_stdout(DisplayStdout, NewData),
      NewOnData = on_data(OnData, NewData),
      if
        DisplayStdout -> io:format("~s", [NewData]);
        true -> ok
      end,
      loop(Port, Data ++ NewData, Timeout, StdoutOnError, DisplayStdout, NewOnData);
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, S}} ->
      if
        StdoutOnError -> {error, S, Data};
        true -> {error, S}
      end
  after
    Timeout ->
      {error, timeout}
  end.

display_stdout(true, NewData) -> io:format("~ts", [NewData]);
display_stdout(_, _NewData) -> ok.

on_data({Function, State}, NewData) when is_function(Function, 2) ->
  NewState = erlang:apply(Function, [NewData, State]),
  {Function, NewState};
on_data(Function, NewData) when is_function(Function, 1) ->
  erlang:apply(Function, [NewData]),
  Function;
on_data(Function, _NewData) ->
  Function.

run_all([], Options, Acc) ->
  Results = lists:reverse(Acc),
  case lists:keyfind(return, 1, Options) of
    {return, Type, all} ->
      {ok, results(Type, Results)};
    {return, Type, last} ->
      {ok, results(Type, [lists:nth(length(Results), Results)])};
    {return, Type, N} when is_integer(N) ->
      {ok, results(Type, nths([N], Results))};
    {return, Type, L} when is_list(L) ->
      {ok, results(Type, nths(L, Results))};
    _ ->
      {ok, results(?DEFAULT_RETURN_TYPE, Results)}
  end;
run_all([Cmd|Cmds], Options, Acc) ->
  case run(Cmd, Options) of
    {ok, Data} -> run_all(Cmds, Options, [Data|Acc]);
    E -> E
  end.

nths(Elements, List) ->
  lists:reverse(
    lists:foldl(fun
                  (N, Acc) when N =< length(List), N >= 0 ->
                  [lists:nth(N, List)|Acc];
                (_, Acc) ->
                  Acc
                end, [], Elements)).

results(combined, List) ->
  lists:flatten(List);
results(_, List) -> List.

format_command({Cmd, Args}, Cgexec, Cgroup) when is_list(Args) ->
  run_in_cgroup(bucs:to_string(lists:flatten(io_lib:format(Cmd, Args))), Cgexec, Cgroup);
format_command(Cmd, Cgexec, Cgroup) ->
  run_in_cgroup(bucs:to_string(Cmd), Cgexec, Cgroup).

run_in_cgroup(Cmd, _Cgexec, undefined) ->
  Cmd;
run_in_cgroup(Cmd, Cgexec, Cgroup) ->
  lists:flatten(io_lib:format(?CGEXEC, [Cgexec, Cgroup, Cmd])).

%% @doc
%% Execute the given function function in the given path.
%%
%% Example :
%%
%% <pre lang="erlang">
%% bucos:in("/tmp", fun() ->
%%   ?assertMatch({ok, "/tmp"}, file:get_cwd())
%%   end).
%% </pre>
%% @end
in(Path, Fun, Args) when is_function(Fun, length(Args)) ->
  case file:get_cwd() of
    {ok, Dir} ->
      case file:set_cwd(Path) of
        ok ->
          Result = apply(Fun, Args),
          case file:set_cwd(Dir) of
            ok -> Result;
            E -> E
          end;
        E -> E
      end;
    E -> E
  end.

%% @equiv in(Path, Fun, [])
in(Path, Fun) when is_function(Fun) ->
  in(Path, Fun, []).
