%%% vim:sw=2:ts=2:et
%%%-----------------------------------------------------------------------------
%%% @doc This file is the source of the `fixdump' escript built by the `rebar'.
%%%
%%% Parses a FIX log file produced by fix_logger and outputs it in a human
%%% readable format.
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%%-----------------------------------------------------------------------------
-module(fix_dump_es).
-export([main/1]).

%-mode(debug).
-mode(compile).

main(Args) ->
  load_paths(),
  try
    fix_dump:print(Args)
  catch throw:What ->
    case What of
      {stop,I} -> halt(I);
      usage    -> halt(1)
    end
  end.

load_paths() ->
  Config = env:subst_env_path("~/.config/fixdumprc"),
  case filelib:is_regular(Config) of
    true ->
      case file:consult(Config) of
        {ok, [Paths]} ->
          [try_add_path(P, Config) || P <- Paths], ok;
        {error, What} ->
          io:format("Error in config file ~s: ~p\n", [Config, What]),
          halt(1)
      end;
    false ->
      ok
  end.

try_add_path(Path, Config) ->
  Path1 = env:subst_env_path(Path),
  case filelib:is_dir(Path1) of
    true ->
      code:add_patha(Path1);
    false ->
      io:format(standard_error, "File ~s contains bad path: ~p\n", [Config, Path]),
      halt(1)
  end.
