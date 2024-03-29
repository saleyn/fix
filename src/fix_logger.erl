%%%-----------------------------------------------------------------------------
%%% @doc FIX file logger
%%%
%%% This module implements a logger that can be used for writing FIX transaction
%%% messages to a file that optionally gets automatically rotated by date or
%%% by size.
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2009 Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2009-12-02
%%%-----------------------------------------------------------------------------
-module(fix_logger).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% External exports
-export([start_link/6, start/6, close/1, log/3, log/4, log/5, write/2]).
-export([filename/1, now/0, archive/5]).

%% gen_server callbacks
-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3
]).

-export_type([options/0]).

-compile({no_auto_import,[now/0]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

-define(FILE_OPTS, [append, raw, binary, {delayed_write, 4096, 500}]).

-type vars()      :: [{atom, list()|binary()}].
-type direction() :: in|out|error|undefined.

-type options() :: #{
  xchg       => binary(),
  variant    => atom(),
  app        => atom(),
  prefix     => binary(),
  logpfx     => binary(),
  vars       => vars(),
  on_new     => on_new(),
  dir_fun    => fun((direction()) -> binary()|list()),
  rotate     => date|{size, Sz::integer()}|none,
  keep_files => N::integer(),
  utc        => boolean()
}.
%% Options passed to logger at startup:
%% <dl>
%% <dt>`{xchg, Exchange::binary()}'</dt>
%%   <dd>Exchange name for this log</dd>
%% <dt>`{variant, atom()}'</dt>
%%   <dd>FIX variant that produces this log</dd>
%% <dt>`{app, atom()}'</dt>
%%   <dd>FIX variant application name that produces this log</dd>
%% <dt>`{prefix, Prefix::binary()}'</dt>
%%   <dd>Prefix name to use when logging to a log file</dd>
%% <dt>`{logpfx, LogPfx::binary()}'</dt>
%%   <dd>Prefix name to use in calls to the system logger</dd>
%% <dt>`{vars, [{Var::atom(), Val::string()|binary()}]}'</dt>
%%   <dd>Variable bindings used in filename substitution. E.g. [{type, "oe"}],
%%       when Filename = "/tmp/%Y%m%d-krx-${type}.log"
%%   </dd>
%% <dt>`{on_new, ignore|string()|binary()|fun((Filename::binary()) -> Res)}'</dt>
%%   <dd>Content to write to newly created file. It can be an atom `ignore',
%%       a string, a binary, or a function that returns binary or atom `ignore'.
%%       In the last case the returned value may contain variable names and
%%       date/time templates (similar to the ones used in defining a filename),
%%       which will be substituted using current time, and values passed in the
%%       `vars' option.
%%   </dd>
%% <dt>`dir_fun'</dt>
%%   <dd>Direction-formatting function `fun(in|out|error|undefined) -> string()'.
%%       By default the direction is printed as: `" <- "' for `in', `" -> "'
%%       for `out', and `"    "' for `undefined', and `" !! "' for error.
%%   </dd>
%% <dt>`rotate'</dt>
%%   <dd>Rotate logs by `date' or by file size `{size, Sz::integer()}'</dd>
%% <dt>`keep_files'</dt>
%%   <dd>Keep `N' number of rotated files (only for Rotate of `{size, Sz}')</dd>
%% <dt>`utc'</dt><dd>Write time in UTC timezone</dd>
%% </dl>

-type on_new() ::
  fun((Filename::string()) -> ignore|string()|binary())|ignore|string()|binary().
-type dir_fun() :: fun((direction()) -> string()).

-record(state, {
  fd,                                           %% term(), file descriptor
  prefix=""     :: string(),                    %% Prefix for logged messages
  logpfx        :: string(),                    %% Prefix for the logger:*/2
  fname         :: string()|undefined,          %% file name
  fname_mask    :: string(),                    %% file name source mask
  %% Execute this fun or write to file on creating a new file
  on_new        :: on_new(),
  dir_fun       :: undefined|dir_fun(),         %% Direction-formatting lambda
  date          :: erlang:date(),               %% Last written date
  sz_chk=0      :: integer(),                   %% Last file size checking time
  rotate        :: none|date|{size, integer()}, %% File rotation
  keep_files=5  :: integer(),                   %% Max number of rotated files
  xchg          :: binary()|string(),           %% Exchange name
  variant       :: atom(),                      %% FIX variant
  app           :: atom(),                      %% FIX variant application name
  vars          :: vars(),                      %% Bindings for filename
  utc           :: boolean()                    %% Write time in UTC timezone
}).

%%%-----------------------------------------------------------------------------
%%% External functions
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Called by a supervisor to start the logging process.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(Name::atom(), App::atom(), Variant::atom(),
                 Xchg::string()|binary(), string(), Options::options()) ->
        {ok, Pid::pid()} | {error, Reason :: any()}.
start_link(Name, App, Variant, Xchg, Filename, Options) ->
  startit(Name, start_link, App, Variant, Xchg, Filename, Options).

-spec start(Name::atom(), App::atom(), Variant::atom(),
            Xchg::string()|binary(), string(), Options::options()) ->
        {ok, Pid::pid()} | {error, Reason :: any()}.
start(Name, App, Variant, Xchg, Filename, Options) ->
  startit(Name, start, App, Variant, Xchg, Filename, Options).

startit(Name, F, App, Variant, Xchg, Filename, Opts)
  when is_atom(Name), is_atom(F), is_atom(App), is_atom(Variant),
      (is_list(Xchg) orelse is_binary(Xchg)), is_map(Opts) ->
  Opts1 = maps:merge(Opts,
                    #{filename => Filename, app  => App,
                      variant  => Variant,  xchg => Xchg, name => Name}),
  gen_server:F({local, Name}, ?MODULE, Opts1, []).

%%------------------------------------------------------------------------------
%% @doc Close currently open log file.
%% @end
%%------------------------------------------------------------------------------
close(Logger) ->
    gen_server:cast(Logger, close).

%%------------------------------------------------------------------------------
%% @doc Get filename used by the logger.
%% @end
%%------------------------------------------------------------------------------
filename(Logger) ->
    gen_server:call(Logger, filename).

%%------------------------------------------------------------------------------
%% @doc Save `Data' to log file. The call is asynchronous.
%% @end
%%------------------------------------------------------------------------------
-spec log(pid(), direction()|string(),
          list()|binary()|fun(() -> list()|binary())) -> ok.
log(Logger, Dir, Data) when is_atom(Dir) ->
  log2(Logger, {op(Data), now(), direction(Dir), Data});
log(Logger, Fmt, Args) when is_list(Fmt), is_list(Args) ->
  log2(Logger, {log, now(), undefined, Fmt, Args}).

-spec log(pid(), direction(),
          list()|binary()|
          list() | fun((list()) -> list()|binary()),
          list()) -> ok.
log(Logger, Dir, Fun, Args) when is_function(Fun, 1), is_list(Args) ->
  log2(Logger, {log_fun, now(), direction(Dir), Fun, Args});
log(Logger, Dir, Fmt, Args) when is_list(Fmt), is_list(Args) ->
  log2(Logger, {log, now(), direction(Dir), Fmt, Args}).

%%------------------------------------------------------------------------------
%% @doc Save result of `io_lib:format(Fmt, Args)' to file.
%% @end
%%------------------------------------------------------------------------------
-spec log(pid()|atom(), direction(), string(),list(), MS::integer()) -> ok.
log(Logger, Dir, Fmt, Args, Now) ->
    log2(Logger, {log, now(Now), direction(Dir), Fmt, Args}).

%% @doc Write verbatim data to log file
write(Logger, Data) ->
  log2(Logger, {log, undefined, undefined, Data}).

%% @doc Add files matching FileMask to a zip file `ZipFile', where files
%%      must be older than the given `FromTS' timestamp.
%%
%% Example:
%% <pre>
%% 1> fix_logger:archive("log/%Y%m%d-one.log", "log/%Y%m.one.zip",
%%      erlang:system_time(millisecond)-86400000*3, false, []).
%% {ok, <<"log/202207.one.zip">>, [<<"log/20220625-one.log">>]}
%% </pre>
archive(FileMask, ZipFile, FromTS, UTC, Bindings)
    when is_binary(FileMask), is_binary(ZipFile)
       , is_integer(FromTS), is_boolean(UTC), is_list(Bindings) ->
  FileML =  to_list(FileMask),
  FileM  =  lists:foldl(fun ({M,Rep}, FM) ->
                          lists:flatten(string:replace(FM, M, Rep))
                        end,
                        FileML,
                        [{"%Y", "????"}, {"%m", "??"}, {"%d", "??"}]),
  {DT,_}   = split_time(FromTS, UTC),
  FileDT   = to_list(replace(FileML, DT, Bindings)),
  Zip      = replace(to_list(ZipFile), DT, []),
  case filelib:ensure_dir(FileDT) of
    ok ->
      Files = [F || F <- filelib:wildcard(FileM)
                 ,  F <  FileDT],
      {ok, Zip} = zip:create(Zip, Files),
      [file:delete(F) || F <- Files],
      {ok, list_to_binary(Zip), [list_to_binary(F) || F <- Files]};
    {error, Reason} ->
      {error, Reason}
  end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Called by gen_server framework at process startup.
%%      ```
%%      Args      = [Filename, Verbosity]
%%      Filename  = string()  - is log filename.
%%      Verbosity = boolean() - controls printouts to console.
%%      '''
%% @end
%% @private
%%------------------------------------------------------------------------------
init(Opts) ->
  try
    File    = to_list(maps:get(filename, Opts)),
    Xchg    = maps:get(xchg,       Opts, undefined),
    Var     = maps:get(variant,    Opts, undefined),
    App     = maps:get(app,        Opts, undefined),
    Vars    = maps:get(vars,       Opts, []),
    OnNew   = maps:get(on_new,     Opts, ""),
    Dir     = maps:get(dir_fun,    Opts, undefined),
    UTC     = maps:get(utc,        Opts, false),
    Rotate  = maps:get(rotate,     Opts, date),
    Keep    = maps:get(keep_files, Opts, 5),
    Name    = atom_to_list(maps:get(name,Opts)),
    {Pfx,B} = case maps:find(prefix,Opts) of
                error      -> {" " ++ Name, true};
                {ok, none} -> {"",  false};
                {ok,   ""} -> {"",  false};
                {ok,  Str} -> {to_list(Str), false}
              end,
    LogPfx  = case maps:find(logpfx,Opts) of
                error when B -> Name ++ ": ";
                error        -> Pfx  ++ ": ";
                {ok, none}   -> "";
                {ok,   ""}   -> "";
                {ok, Str2}   -> to_list(Str2) ++ ": "
              end,
    (is_binary(Xchg) orelse is_list(Xchg))
      orelse throw("xchg parameter missing"),
    Var == undefined andalso throw("var parameter missing"),
    is_atom(Var)      orelse throw("var parameter must be an atom"),
    App == undefined andalso throw("app parameter missing"),
    is_atom(App)      orelse throw("app parameter must be an atom"),
    (is_list(Vars)
      andalso lists:foldl(fun({K,V}, A) ->
                            A and is_atom(K) and is_string(V)
                          end, true, Vars))
      orelse  throw("vars must be of type [{atom, string|binary}]"),
    is_string(OnNew)
      orelse OnNew==ignore
      orelse (is_function(OnNew, 1) andalso
              case OnNew("Test") of
                ignore -> true;
                V      -> is_list(V) orelse is_binary(V)
              end)
      orelse  throw("on_new must be string|list|fun/1 or atom 'ignore'"),
    Dir == undefined
      orelse (is_function(Dir, 1)
              andalso (is_list(Dir(in))        orelse is_binary(Dir(in)))
              andalso (is_list(Dir(out))       orelse is_binary(Dir(out)))
              andalso (is_list(Dir(undefined)) orelse is_binary(Dir(undefined))))
      orelse  throw("dir_fun must be a fun/1 that formats in|out|undefined"),
    (is_integer(Keep) andalso Keep >= 0)
      orelse  throw("keep_files option must be integer > 0"),
    Rotate == date
      andalso string:find(File, "%d") == nomatch
      andalso throw("log filename must contain \"%d\""),

    Vars1 = [{K, to_list(V)} || {K,V} <- Vars],

    {ok, #state{fname_mask=File, on_new=OnNew,  utc=UTC,     dir_fun=Dir,
                rotate=Rotate,   vars=Vars1,    keep_files=Keep, app=App,
                prefix=Pfx,      logpfx=LogPfx, xchg=Xchg,   variant=Var}}
  catch throw:Err:ST ->
    Error = "Error starting logger: " ++ Err,
    {stop, {Error, ST}}
  end.

%%------------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%------------------------------------------------------------------------------
handle_call({log_fun,Now,Dir,Fun,Args}, _From, State) when is_function(Fun,1) ->
  NewState = log_to_file(State, Now, Dir, Fun(Args)),
  {reply, ok, NewState};

handle_call({log_fun,Now,Dir,Fun}, _From, State) when is_function(Fun,0) ->
  NewState = log_to_file(State, Now, Dir, Fun()),
  {reply, ok, NewState};

handle_call({log,Now,Dir,Data}, _From, State) ->
  NewState = log_to_file(State, Now,Dir,Data),
  {reply, ok, NewState};

handle_call({log,Now,Dir,Fmt,Args}, _From, State) ->
  Data = io_lib:format(Fmt, Args),
  NewState = log_to_file(State, Now,Dir,Data),
  {noreply, NewState};

handle_call(filename, _From, #state{fname=Filename} = State) ->
  {reply, Filename, State};

handle_call(Cmd, _From, State) ->
  {stop, {unsupported_call, Cmd, State}}.

%%------------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%------------------------------------------------------------------------------
handle_cast({log_fun,Now,Dir,Fun,Args}, State) when is_function(Fun,1) ->
  Data = Fun(Args),
  handle_cast({log,Now,Dir,Data}, State);

handle_cast({log_fun,Now,Dir,Fun}, State) when is_function(Fun,0) ->
  NewState = log_to_file(State, Now,Dir,Fun()),
  {noreply, NewState};

handle_cast({log,Now,Dir,Data}, State) ->
  NewState = log_to_file(State, Now,Dir,Data),
  {noreply, NewState};

handle_cast({log,Now,Dir,Fmt,Args}, State) ->
  Data = io_lib:format(Fmt, Args),
  NewState = log_to_file(State, Now,Dir,Data),
  {noreply, NewState};

handle_cast(close, #state{fd=undefined} = State) ->
  {noreply, State};
handle_cast(close, #state{fd=IO} = State) ->
  file:close(IO),
  {noreply, State#state{fd=undefined}};

handle_cast(Other, #state{logpfx=Pfx} = State) ->
  ?LOG_ERROR("~sunhandled cast: ~p", [Pfx, Other]),
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%------------------------------------------------------------------------------
handle_info(Msg, #state{logpfx=Pfx} = State) ->
  ?LOG_ERROR("~sunhandled message: ~p", [Pfx, Msg]),
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%------------------------------------------------------------------------------
terminate(Reason, _State) ->
  Reason.

%%------------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

log2(Logger, Details) when is_atom(Logger) ->
  case whereis(Logger) of
    Pid when is_pid(Pid) ->
      log3(Pid, Details);
    undefined ->
      erlang:error({no_logger_process, Logger})
  end;

log2(Logger, Details) when is_pid(Logger) ->
  log3(Logger, Details).

log3(Pid, Details) ->
  {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
  Thr = application:get_env(fix, async_log_threshold),
  if Len < Thr ->
    gen_server:cast(Pid, Details);
  true ->
    gen_server:call(Pid, Details)
  end.

op(Data) when is_list(Data)       -> log;
op(Data) when is_binary(Data)     -> log;
op(Data) when is_function(Data,0) -> log_fun.

direction(I) when I==in; I==out; I==error; I==undefined -> I.

now(undefined)  -> undefined;
now(T) when is_integer(T), T > 1577836800000 -> T.  %% 2020-01-01

%% @doc Return system time in number of milleseconds since epoch.
now() -> erlang:system_time(millisecond).

dir(undefined, Dir) ->
  case Dir of
    in    -> " <- ";
    out   -> " -> ";
    error -> " !! ";
    _     -> "    "
  end;
dir(F, Dir) when is_function(F, 1) ->
  F(Dir).

%%------------------------------------------------------------------------------
%% @doc  Save `Data' to a log file.  Opens the log file if it's not already
%%       open and returns its file descriptor.
%% @end
%%------------------------------------------------------------------------------
-spec log_to_file(#state{}, TimeWithMsec::integer(),
                  direction(), file:io_data()) ->
        NewState::#state{}.
log_to_file(State, Now, Dir, Data) ->
  case maybe_open_file(State, Now) of
    {true, DateTime={DT,_}, #state{on_new=OnNew, fname=FN, vars=Vars} = S1} ->
      H0 = if is_function(OnNew, 1) -> OnNew(FN); true -> OnNew end,
      H1 = if
        is_list(H0); is_binary(H0) ->
          replace(to_list(H0), DT, Vars);
        H0==ignore ->
          H0;
        true ->
          ""
      end,
      Hdr =
        if H1 == ignore ->
          "";
        true ->
          io_lib:format(
            "## FIX log: exchange=~s, variant=~s, app=~s~s~s\n",
            [State#state.xchg, atom_to_list(State#state.variant),
             atom_to_list(State#state.app),
             if H1==""; H1 == <<"">> -> undefined; true -> ": " end, H1])
        end,
      State2 = safe_write_file(DateTime, header, Hdr, S1),
      safe_write_file(DateTime, Dir, Data, State2);
    {false, DateTime, S1} ->
      safe_write_file(DateTime, Dir, Data, S1)
  end.

maybe_open_file(#state{fd=undefined, logpfx=Pfx}=S, Now) ->
  FN = filename(Now, S),
  N  = filelib:file_size(FN),
  case file:open(FN, ?FILE_OPTS) of
  {ok, NewDev} ->
    {{Date,_},_} = Time = split_time(Now, S#state.utc),
    Created = N == 0,
    Created andalso ?LOG_INFO("~screated new log file ~s", [Pfx, FN]),
    {Created, Time, S#state{fd=NewDev, fname=FN, date=Date}};
  {error, Reason} ->
    E = file:format_error(Reason),
    ?LOG_ERROR("~serror opening packet log file ~p: ~s", [Pfx, FN, E]),
    erlang:error(FN ++ ": " ++ E)
  end;
maybe_open_file(State, Now) ->
  check_rotate(State, Now).

check_rotate(#state{fd=Dev, utc=UTC, rotate=Rot} = S, Now) when Dev /= undefined ->
  {{Date,_}, _} = Time = split_time(Now, UTC),
  {S1, Rotate, RotType, Info} =
    case Rot of
      date when Date /= S#state.date ->
        {S, true, date, Date};
      {size, Size} when Now - S#state.sz_chk > 15000 ->
        N = filelib:file_size(S#state.fname),
        {S#state{sz_chk=Now}, N > Size, size, N};
      _ ->
        {S, false, undefined, undefined}
    end,
  if Rotate ->
    application:get_env(fix, fix_logger_debug, false) == true andalso
      ?LOG_INFO("~srotating log file (sdate=~p, type=~p, info=~p)",
        [S#state.logpfx, S#state.date, Rot, Info]),
    file:close(Dev),
    rotate_files(RotType, S1),
    maybe_open_file(S1#state{fd=undefined, fname=undefined}, Now);
  true ->
    {false, Time, S1}
  end.

safe_write_file(_DT, header, H, State) when H==""; H == <<"">>; H==undefined ->
  State;
safe_write_file(_DT, header, Data, State) ->
  do_write(State, Data);
safe_write_file(DateTime, Dir, Data, #state{prefix=Pfx, dir_fun=DF} = State) ->
  Time = format_time(DateTime),
  DirL = dir(DF, Dir),
  do_write(State, [Time, Pfx, DirL, Data]).

do_write(State = #state{fd=Dev, logpfx=Pfx}, Data) ->
  case file:write(Dev, Data) of
    ok ->
      State;
    {error, Reason} ->
      ?LOG_ERROR("~serror writing data to log file ~s: ~p",
                [Pfx, State#state.fname, file:format_error(Reason)]),
      erlang:error(Reason)
  end.

%%------------------------------------------------------------------------------
%% @doc Create a filename by replacing `Bindings', `%Y', `%m' and `%d' in the
%%      `Filename'.
%% @end
%%------------------------------------------------------------------------------
-spec filename(Now::integer(), #state{}) -> string().
filename(Now, #state{fname_mask=Fname, utc=UTC, logpfx=Pfx, vars=Bindings}) ->
  {DT,_} = split_time(Now, UTC),
  File = replace(Fname, DT, Bindings),
  case filelib:ensure_dir(File) of
    ok ->
      File;
    {error, err} ->
      ?LOG_ERROR("~scannot ensure directory for file ~s: ~p", [Pfx, File, err]),
      erlang:error({log_file, File, err})
  end.

replace(File, {{Y,M,D},{HH,MM,SS}}, Bindings) ->
  File1 = lists:foldl(
    fun({S,I}, A) -> lists:append(string:replace(A, S, i2lp(I), all)) end,
    File,
    [{"%Y",Y}, {"%m",M}, {"%d",D}, {"%H",HH}, {"%M",MM}, {"%S",SS}]),
  env:subst_env_path(File1, Bindings).

format_time({{{Y,M,D}, {H,Mi,S}}, Msec}) ->
  [integer_to_binary(Y), i2b(M), i2b(D), $-,
   i2b(H), $:, i2b(Mi), $:, i2b(S), $., i2b3(Msec)].

split_time(Now, UTC) when is_integer(Now) ->
  Time = Now div 1000,
  Msec = Now - Time*1000,
  CalTime = erlang:posixtime_to_universaltime(Time),
  if
    UTC  -> {CalTime, Msec};
    true -> {erlang:universaltime_to_localtime(CalTime), Msec}
  end.

is_string(S) -> is_list(S) orelse is_binary(S).

to_list(S) when is_list(S)   -> S;
to_list(S) when is_binary(S) -> binary_to_list(S).

i2b(I) when I < 10    -> <<$0, ($0+I)>>;
i2b(I)                -> integer_to_binary(I).

i2b3(I) when I < 10   -> <<$0, $0, ($0+I)>>;
i2b3(I) when I < 100  -> <<$0, (i2b(I))/binary>>;
i2b3(I) when I < 1000 -> integer_to_binary(I).

i2lp(I) when I < 10   -> [$0, $0+I];
i2lp(I)               -> integer_to_list(I).

rotate_files(RotType, #state{logpfx=Pfx, keep_files=Keep, fname_mask=Fname}=S) ->
  case RotType of
    size ->
      File      = S#state.fname,
      KeepSfxs  = [{"." ++ integer_to_list(I), I} || I <- lists:seq(1, Keep-1)],
      KeepFiles = [{File, 0} | [{File ++ I, J} || {I,J} <- KeepSfxs]],
      AllFiles  = filelib:wildcard(File ++ ".*"),
      OldFiles  = AllFiles -- [F || {F, _} <- KeepFiles],
      %% Remove old log files
      lists:foreach(fun(F) ->
        ok = file:delete(F),
        ?LOG_NOTICE("~sdeleted old log file: ~s", [Pfx, F])
      end, OldFiles),
      %% Rename remaining log files
      lists:foreach(fun({F, I}) ->
        NewFile = File ++ "." ++ integer_to_list(I+1),
        case filelib:is_regular(F) of
          true ->
            case file:rename(F, NewFile) of
              ok ->
                ?LOG_NOTICE("~srotated log file: ~s -> ~s", [Pfx, F, NewFile]);
              {error, E} ->
                ?LOG_NOTICE("~scouldn't move file ~s to ~s: ~p", [Pfx, F, NewFile, E]),
                NewFile1 = F ++ ".bak",
                file:delete(NewFile1),
                file:rename(F, NewFile1)
            end;
          false ->
            ok
        end
      end, lists:reverse(KeepFiles));
    date ->
      case string:find(Fname, "%d") of
        nomatch ->
          OldFile = S#state.fname,
          NewFile = replace("%Y%m%d." ++ OldFile, {date(),time()}, []),
          case file:rename(OldFile, NewFile) of
            ok ->
              ?LOG_NOTICE("~srotated log file: ~s -> ~s", [Pfx, OldFile, NewFile]);
            {error, E} ->
              ?LOG_NOTICE("~scouldn't move file ~s to ~s: ~p", [Pfx, OldFile, NewFile, E]),
              NewFile1 = NewFile ++ ".bak",
              file:delete(NewFile1),
              file:rename(OldFile, NewFile1)
          end;
        _ ->
          %% No need to rotate
          ok
      end
  end.

%%%----------------------------------------------------------------------------
%%% Test Cases
%%%----------------------------------------------------------------------------

-ifdef(EUNIT).

log_test() ->
  DTime   = {{2022,1,2}, {3,4,5}},
  Prefix  = "|SRC:DST|",
  DirFun  = fun
              (in)    -> "I|";
              (out)   -> "O|";
              (error) -> "E|";
              (_)     -> " |"
            end,
  Opts    = #{rotate  => none, utc => true,
              prefix  => Prefix, dir_fun => DirFun,
              on_new  => fun(F) -> "%Y%m%d\n" end},
  Filenm  = "/tmp/%Y%m%d.test.fix.log",
  FName   = replace(Filenm, DTime, []),
  file:delete(FName),

  Logger  = fix_logger:start_link(flt, tmp_app, var, "TMP", Filenm, Opts),
  Now     = erlang:universaltime_to_posixtime(DTime)*1000,
  fix_logger:log(flt, out,   "Test1\n", [], Now),
  fix_logger:log(flt, error, "Test2\n", [], Now),
  FN = fix_logger:filename(flt),
  fix_logger:close(flt),

  ?assertEqual(FName, FN),
  ?assert(filelib:is_regular(FN)),

  {{Y,M,D},{HH,MM,SS}} = DTime,
  {ok, Bin} = file:read_file(FN),
  Expect    = lists:flatten(io_lib:format(
    "## FIX log: exchange=TMP, variant=var, app=tmp_app: ~w~.2.0w~.2.0w\n\n"
    "~w~.2.0w~.2.0w-~.2.0w:~.2.0w:~.2.0w.000~sO|Test1\n"
    "~w~.2.0w~.2.0w-~.2.0w:~.2.0w:~.2.0w.000~sE|Test2\n",
    [Y,M,D,
     Y,M,D,HH,MM,SS, Prefix,
     Y,M,D,HH,MM,SS, Prefix])),

  ?assertEqual(Expect, to_list(Bin)),
  file:delete(FName).

-endif.
