#!/usr/bin/env escript
%%% vim:sw=2:ts=2:et
%%! -env ERL_CRASH_DUMP /dev/null
%%%-----------------------------------------------------------------------------
%%% @doc FIX log file parser
%%%
%%% Parses a FIX log file produced by fix_logger and outputs it in a human
%%% readable format.
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%%-----------------------------------------------------------------------------
-module(fixdump).
-export([main/1]).

%-mode(debug).
-mode(compile).

main(Args) ->
  load_paths(),
  try
    fix_dump:print(Args)
  catch throw:usage ->
    halt(1)
  end.

load_paths() ->
  Home   = os:getenv("HOME"),
  Config = filename:join(Home, ".config/fixdumprc"),
  case filelib:is_regular(Config) of
    true ->
      case file:consult(Config) of
        {ok, [Paths]} ->
          [try_add_path(P, Config, Home) || P <- Paths], ok;
        {error, What} ->
          io:format("Error in config file ~s: ~p\n", [Config, What]),
          halt(1)
      end;
    false ->
      ok
  end.

try_add_path(Path, Config, Home) ->
  Path1 = replace_home(Path, Home),
  case filelib:is_dir(Path1) of
    true ->
      code:add_patha(Path1);
    false ->
      io:format(standard_error, "File ~s contains bad path: ~p\n", [Config, Path]),
      halt(1)
  end.

replace_home(Path, Home) ->
  case re:run(Path,
      "(?|(?:\\$\\$)|(?:~[^/$]*)|(?:\\${[A-Za-z][A-Za-z_0-9]*})|(?:\\$[A-Za-z][A-Za-z_0-9]*))",
      [global, {capture, all}])
  of
    {match, List} ->
      lists:foldl(fun([{Pos,Len}], P) ->
        string:substr(P, 1, Pos) ++ Home ++ string:substr(P, Pos+1+Len)
      end, Path, List);
    nomatch ->
      Path
  end.
