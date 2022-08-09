%%%-----------------------------------------------------------------------------
%%% @doc FIX log file parser
%%%
%%% Parses a FIX log file produced by `fix_logger' and outputs it in a human
%%% readable format.
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2012-07-09
%%%-----------------------------------------------------------------------------
-module(fix_dump).
-export([print/1, print/2, print/5]).

-mode(compile).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/zip.hrl").

-record(args, {
  file,
  xchg,
  variant,
  app :: atom(),
  show_raw      = false,
  skip_comments = true,
  line          = 1,
  errors        = 0,
  msg_type_tag,
  from_line     = 1,
  to_line,
  line_count,
  filters       = []
}).

print(Args) when is_map(Args) ->
  Args1   = #args{
    xchg          = maps:get(xchg,          Args, undefined),
    variant       = maps:get(variant,       Args, undefined),
    app           = maps:get(app,           Args, undefined),
    show_raw      = maps:get(show_raw,      Args, false),
    skip_comments = maps:get(skip_comments, Args, true),
    from_line     = maps:get(from,          Args, 1),
    to_line       = maps:get(to,            Args, undefined),
    line_count    = maps:get(count,         Args, undefined),
    filters       = maps:get(filters,       Args, []),
    line          = 1,
    errors        = 0
  },
  print(Args1#args.app, Args1);

print(Args) when is_list(Args) ->
  print(undefined, Args).

print(FixApp, Args) when is_atom(FixApp), is_list(Args) ->
  print(FixApp, parse(Args, #args{}));

print(FixApp, #args{file=F, app=App, from_line=FL, line_count=LC} = Args)
  when is_atom(FixApp) ->
  ToLine = if is_integer(FL), is_integer(LC) -> FL+LC-1; true -> Args#args.to_line end,
  State  = Args#args{to_line=ToLine, app = if App /= undefined -> App; true -> FixApp end},
  %% Enable unicode output
  io:setopts(standard_io, [{encoding, unicode}, binary]),
  if F==""; F==undefined; F=="-" ->
    read_file(standard_io, State);
  true ->
    case file:open(F, [binary, raw]) of
      {ok, IO} ->
        try
          read_file(IO, State)
        after
          file:close(IO)
        end;
      {error, Reason} ->
        erlang:error(lists:flatten(io_lib:format("Cannot open file ~s: ~p", [F, Reason])))
    end
  end.

print(FixApp, Xchg, Variant, App, Args = #args{})
  when (is_list(Xchg) orelse is_binary(Xchg)), is_atom(Variant), is_atom(App) ->
  print(FixApp, Args#args{xchg=Xchg, variant=Variant, app=App}).

usage() ->
  io:format(
    standard_error,
    "Usage: ~s -f File\n"
    "Options:\n"
    "  -f File                  - FIX log file\n"
    "  -x Xchg                  - Exchange name\n"
    "  -v Variant               - FIX Variant\n"
    "  -a App                   - FIX Variant application\n"
		"  -r|--raw                 - Show raw FIX field values\n"
    "  -c|--comments            - Print comment lines that begin with '#'\n"
    "  -L|--list                - List content of the archive to stdout\n"
    "  -l|--lines From To       - Print decoded range of lines: From..To\n"
    "  -from|--from-line  From  - Print range of lines begining with From\n"
    "  -to  |--to-line    From  - Print range of lines begining with From\n"
    "  -count|--line-count Cnt  - Print at most Cnt lines\n"
    "  -filter|--filter Exp     - Filter field. Exp: 'Field=Value'\n"
    "\n"
    "In order for this script to be able to parse log files of a FIX variant,\n"
    "the path to the FIX variant application must be added to a resource file\n"
    "identified by the environment variable FIXDUMP_RC (which defaults to \n"
    "~~/.config/fixdumprc):\n"
    "\n"
    "  ~~/.config/fixdumprc\n"
    "  ===================\n"
    "  [\n"
    "    %% Paths may contain \"~~\", \"$HOME\", \"${HOME}\" variables\n"
    "    \"~~/path/to/fix\",\n"
    "    \"~~/path/to/fix_variant_app\"\n"
    "  ].\n"
    "\n"
    "Examples\n"
    "========\n"
    "  Parse FIX log file\n"
    "    ~s -f 20220102-oe1.fix.log\n\n"
    , lists:duplicate(2, filename:basename(escript:script_name()))),
  throw(usage).

parse(["-f", File     | T], A) -> parse(T, A#args{file    = File});
parse(["-x", Xchg     | T], A) -> parse(T, A#args{xchg    = to_bin(Xchg)});
parse(["-v", Var      | T], A) -> parse(T, A#args{variant = to_atom(Var)});
parse(["-a", App      | T], A) -> parse(T, A#args{app     = to_atom(App)});
parse(["-c"           | T], A) -> parse(T, A#args{skip_comments = false});
parse(["--comments"   | T], A) -> parse(T, A#args{skip_comments = false});
parse(["-L"           | _], _) -> list_archive(), throw({stop, 1});
parse(["-r"           | T], A) -> parse(T, A#args{show_raw      = true});
parse(["--raw"        | T], A) -> parse(T, A#args{show_raw      = true});
parse([Cmd,Fr,To|T], A) when Cmd=="-l"; Cmd=="--lines" ->
  I1=to_int(Cmd,Fr), I2=to_int(Cmd,To),
  parse(T, A#args{from_line = I1, to_line = I2});
parse([Cmd,N|T], A) when Cmd=="-from";  Cmd=="--from-line"  -> I1=to_int(Cmd,N), parse(T, A#args{from_line  = I1});
parse([Cmd,N|T], A) when Cmd=="-to";    Cmd=="--to-line"    -> I1=to_int(Cmd,N), parse(T, A#args{to_line    = I1});
parse([Cmd,N|T], A) when Cmd=="-count"; Cmd=="--line-count" -> I1=to_int(Cmd,N), parse(T, A#args{line_count = I1});
parse(["-filter", Exp | T], A) -> parse(T, A#args{filters = [to_bin(Exp)|A#args.filters]});
parse(["--filter",Exp | T], A) -> parse(T, A#args{filters = [to_bin(Exp)|A#args.filters]});
parse(["-h"           | _], _) -> usage();
parse(["--help"       | _], _) -> usage();
parse([Other          | _], _) ->
  erlang:error(lists:flatten(io_lib:format("Invalid option: ~p", [Other])));
parse([],                   A) -> A.

list_archive() ->
  {ok, ZipPropList} = escript:extract(escript:script_name(), []),
  ZipArchiveBin     = proplists:get_value(archive, ZipPropList),
  {ok, ZipContent}  = zip:table(ZipArchiveBin),

  Entries = [_|_] =
    stringx:align_rows(
      [{"Filename", " | ", "Size", " | ", "Compressed", " | ", "LastModified\n"}] ++
      [{N,          " | ", Sz,     " | ", CSz,          " | ", datetime(Tm)++"\n"}
       || #zip_file{name=N, info = #file_info{size=Sz, mtime=Tm}, comp_size=CSz} <- ZipContent],
      [{return, list}, {ignore_empty, true}, {prefix, "  "},
       {pad, [{3, leading}, {5, leading}, {last, none}]}]),
  [io:format(I) || I <- Entries].

datetime({{Y,M,D},{HH,MM,SS}}) ->
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y,M,D,HH,MM,SS])).

print_line(<<"## FIX log: ", Rest/binary>>=Line,
           #args{line=1, xchg=Xchg0, variant=Var0, app=App0, skip_comments=Skip} = State) ->
  check_line_range(State) orelse Skip orelse io:format("~.8.0w: ~s\n", [1, Line]),
  case re:run(Rest, "exchange=([^ ]+), variant=([a-zA-Z0-9_-]+), app=([a-zA-Z0-9_-]+).*",
              [{capture, all_but_first, list}]) of
    {match, [Xchg, Var, App]} ->
      State0 = State#args{xchg=list_to_binary(Xchg), variant=list_to_atom(Var), app=list_to_atom(App)},
      load_fix_variant(State0#args{line=2});
    nomatch when Xchg0==undefined ->
      missing("-x");
    nomatch when Var0==undefined ->
      missing("-v");
    nomatch when App0==undefined ->
      missing("-a");
    nomatch ->
      load_fix_variant(State#args{line=2})
  end;
print_line(Line, #args{line=1, xchg=Xchg0, variant=Var0, app=App0, skip_comments=Skip} = State) ->
  if Xchg0==undefined ->
    missing("-x");
  Var0==undefined ->
    missing("-v");
  App0==undefined ->
    missing("-a");
  true ->
    check_line_range(State) orelse Skip orelse io:format("~.8.0w: ~s\n", [1, Line]),
    load_fix_variant(State#args{line = 2})
  end;
print_line(<<"#", _/binary>>=Line, #args{line = LineNo, skip_comments = Skip} = State) ->
  check_line_range(State) orelse Skip orelse io:format("~.8.0w: ~s\n", [LineNo, Line]),
  State#args{line = LineNo+1};

print_line(Line, State) ->
  case check_line_range(State) of
    true  -> print_line2(Line, State);
    false -> State#args{line = State#args.line+1}
  end.

print_line2(<<"8=FIX", _/binary>>=Line, State) ->
  print_line3([], Line, State);

print_line2(Line, State) when is_binary(Line) ->
  [Time, L1] = binary:split(Line, <<" ">>),
  [OE,   L2] = binary:split(L1,   <<" ">>),
  [Dir,BMsg] = binary:split(L2,   <<" ">>),
  print_line3([Time, " ", OE, " ", Dir, " "], BMsg, State).

print_line3(Pfx, Line, State) ->
  case check_filter(Line, State#args.filters) of
    true  ->
      try
        {ok, _, Fields} = fix_nif:split(State#args.variant, Line, [{float, binary}, {time, binary}]),
        print_line4(Pfx, Line, Fields, State)
      catch _:Err ->
        io:format("~.8.0w: Error decoding message: ~p\n  ~s\n", [State#args.line, Err, Line]),
        State#args{line = State#args.line+1, errors = State#args.errors+1}
      end;
    false ->
      State#args{line = State#args.line+1}
  end.

print_line4(Pfx, BMsg, Fields, #args{line=LineNo, xchg=Xchg, msg_type_tag=MTT, show_raw=Raw} = State) ->
  MsgType =
    case lists:keyfind(MTT, 1, Fields) of
      {_, Val, _, _} -> Val;
      false          -> ""
    end,
  ML   = lists:foldl(fun(T,S) -> max(length(val(element(1,T))),S) end, 25, Fields),
  RWid = 22,
  Sep  = hpad("", 11+ML+5+11+16 + (if Raw -> RWid+3+3; true -> 0 end)),
  io:format("~ts\n~.8.0w: ~s~s: ~s\n", [Sep, LineNo, Pfx, Xchg, val(MsgType)]),
  {Fmt, Args} =
    if Raw ->
      io:format("──No─┬───Tag─┬─~ts─┬─~ts─┬─~ts\n",
               [hpad("Field", ML), hpad("Binary Value", RWid+3), hpad("Decoded Value", 25)]),
      {"  ~.2.0w │ ~.5w │ ~.*s │ ~-*s~s │ ~s\n",
       fun(I,Tag,K,V,{_,Len}=PosLen) ->
         [I,Tag,ML,K, RWid,binary:part(BMsg, PosLen), if Len>RWid -> "..."; true -> "   " end, V]
       end};
    true ->
      io:format("──No─┬───Tag─┬─~ts─┬─~ts\n",
               [hpad("Field", ML), hpad("Decoded Value", 25)]),
      {"  ~.2.0w │ ~.5w │ ~.*s │ ~s\n",
       fun(I,Tag,K,V,_PosLen) -> [I,Tag,ML,K,V] end}
    end,
  lists:foldl(fun({K,V,Tag,PosLen}, I) ->
    io:format(Fmt, Args(I, Tag, val(K), val(V,BMsg), PosLen)),
    I+1
  end, 1, Fields),
  State#args{line = LineNo+1}.

check_line_range(#args{line=I, from_line=F, to_line=T}) ->
  ( not is_integer(F) orelse I >= F) andalso
  ((not is_integer(T) orelse I =< T)).

check_filter(_Line,[])      -> true;
check_filter(Line, Filters) ->
  lists:dropwhile(
    fun(Filter) -> binary:match(Line, Filter) == nomatch end,
    Filters
  ) /= [].

load_fix_variant(State = #args{app=App, filters=FF}) ->
  % Load the FIX variant for this file
  fix_nif:load_variant(App),

  % Get MsgType field atom ('MsgType' or 'Elixir.MsgType')
  State1 = State#args{msg_type_tag = fix_nif:tag_to_field(App, 35)},

  % Update filters
  Filters = lists:map(fun
    G({K,V}) when is_binary(K) ->
      Key =
        try        _ = binary_to_integer(K), K
        catch   _:_ ->
          try          fix_nif:field_to_tag(App, K)
          catch _:_ -> erlang:error(lists:flatten(
            io_lib:format("Invalid key '~s' in filter (app=~w)", [K, App])))
          end
        end,
      binary:compile_pattern(<<"|", Key/binary, "=", V/binary, "|">>);

    G(Exp) when is_binary(Exp) ->
      [K,V] =
        try          binary:split(Exp, <<"=">>)
        catch _:_ -> erlang:error("Argument -filter must be in format: Field=Value")
        end,
      G({K,V})
  end, FF),
  State1#args{filters=Filters}.

missing(Arg) ->
  erlang:error(
    "Argument "++Arg++" is required or the first line of the log must contain them").

val(A) when is_atom(A) ->
  case atom_to_list(A) of
    "Elixir." ++ S -> S;
    S              -> S
  end;
val(V) when is_binary(V) ->
  V;
val(V) when is_integer(V) ->
  integer_to_list(V);
val(V) ->
  V.

val({_Pos,_Len}=Part, Bin) ->
  binary:part(Bin, Part);
val(Part, _Bin) ->
  val(Part).

hpad(L, Wid) ->
  N = length(L),
  [L, string:copies("─", Wid-N)].

to_int(Cmd,I) when is_list(Cmd)->
  to_int(list_to_binary(Cmd), I);
to_int(Cmd,I) ->
  try list_to_integer(I)
  catch _:_ ->
    erlang:error("Argument " ++ Cmd ++ " must be an integer!")
  end.

to_atom(S) when is_list(S)   -> list_to_atom(S);
to_atom(S) when is_binary(S) -> binary_to_atom(S).

to_bin(S)  when is_list(S)   -> list_to_binary(S);
to_bin(S)  when is_binary(S) -> S.

read_file(IO, State) ->
	case file:read_line(IO) of
		{ok, Line} ->
			State1 = print_line(Line, State),
			read_file(IO, State1);
		eof ->
			ok
	end.
