%%%-----------------------------------------------------------------------------
%%% @doc FIX file logger
%%%
%%% This module implements a logger that can be used for writing FIX transaction
%%% messages to a file that optionally gets automatically rotated by date or
%%% by size.
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2009-12-02
%%%-----------------------------------------------------------------------------
-module(fix_logger).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% External exports
-export([start_link/3, start/3, close/1, log/3, log/4, log/5, write/2, filename/1]).
-export([split_time/2]).

%% gen_server callbacks
-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3
]).

-export_type([options/0]).

-compile({no_auto_import,[now/0]}).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

-record(state, {
  fd,                             %% term(), file descriptor
  prefix=""  :: string(),         %% Prefix for system log messages
  fname      :: string(),         %% file name
  fname_mask :: string(),         %% file name source mask
  %% Execute this fun or write to file on creating a new file
  on_new  :: fun((Filename::string(), Now::erlang:timestamp()) ->
                  {erlang:timestamp(), string()|binary()})
            | string() | binary(),
  date    :: erlang:date(),       %% Last written date
  sz_chk=0:: integer(),           %% Last file size checking time
  rotate  :: none|date|{size, integer()},  %% File rotation
  keep_files=5 :: integer(),      %% Keep this number of rotated files
  vars    :: [{atom(), list()}],  %% Bindings for filename
  utc     :: boolean()            %% Write time in UTC timezone
}).

-define(FILE_OPTS, [append, raw, binary, {delayed_write, 4096, 500}]).

-type options() :: #{
  vars   => [{atom(), string()}],
  on_new => on_new(),
  rotate => date|{size, integer()}|none,
  utc    => boolean()
}.
%% Options passed to logger at startup:
%% <dl>
%% <dt>vars</dt>
%%   <dd>Variable bindings used in filename substitution. E.g. [{type, "oe"}],
%%       when Filename = "/tmp/%Y%m%d-krx-${type}.log"
%%   </dd>
%% <dt>on_new</dt>
%%   <dd>Content to write to newly created file. It can be a string, or a
%%       binary
%%   </dd>
%% </dl>
%% <dt>{rotate, Rotate}</dt>
%%   <dd>Rotate logs by date (Rotate :: `date') or by file size
%%       (Rotate :: {size, integer()})
%%   </dd>
%% <dt>{keep_files, N::integer()}</dt>
%%   <dd>Keep this number of rotated files (only for Rotate of `{size, N}')</dd>
%% <dt>utc</dt><dd>Write time in UTC timezone</dd>
%% </dl>

-type on_new() :: list()|binary().

%%%-----------------------------------------------------------------------------
%%% External functions
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Called by a supervisor to start the logging process.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(Name::atom(), string(), Options::options()) ->
        {ok, Pid::pid()} | {error, Reason :: any()}.
start_link(Name, Filename, Options) ->
  startit(Name, start_link, Filename, Options).

-spec start(Name::atom(), string(), Options::options()) ->
        {ok, Pid::pid()} | {error, Reason :: any()}.
start(Name, Filename, Options) ->
  startit(Name, start, Filename, Options).

startit(Name, F, Filename, Opts) when is_atom(Name), is_atom(F), is_map(Opts) ->
  Map = case maps:find(prefix, Opts) of
          {ok, none} -> Opts#{prefix => ""};
          {ok, _}    -> Opts;
          error      -> Opts#{prefix => atom_to_list(Name)}
        end,
  gen_server:F({local, Name}, ?MODULE, maps:put(filename, Filename, Map), []).

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
-spec log(pid(), in|out|undefined, list()|binary()|fun(() -> list()|binary())) -> ok.
log(Logger, Dir, Data) when is_atom(Dir) ->
  log2(Logger, {op(Data), now(), direction(Dir), Data});
log(Logger, Fmt, Args) when is_list(Fmt), is_list(Args) ->
  log2(Logger, {log, now(), undefined, Fmt, Args}).

-spec log(pid(), in|out|undefined,
          list()|binary()|
          list() | fun((list()) -> list()|binary()),
          list()) -> ok.
log(Logger, Dir, Fun, Args) when is_function(Fun, 1), is_list(Args) ->
  log2(Logger, {log_fun, now(), direction(Dir), Fun, Args});
log(Logger, Dir, Fmt, Args) when is_list(Fmt), is_list(Args) ->
  log2(Logger, {log, now(), direction(Dir), Fmt, Args}).

%% @doc Write verbatim data to log file
write(Logger, Data) ->
    log2(Logger, {log, undefined, undefined, Data}).

%%------------------------------------------------------------------------------
%% @doc Save result of `io_lib:format(Fmt, Args)' to file.
%% @end
%%------------------------------------------------------------------------------
log(Logger, Dir, Fmt, Args, Now) ->
    log2(Logger, {log, now(Now), direction(Dir), Fmt, Args}).

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
-spec init(map()) ->
        {ok, #state{}} | {ok, #state{}, Timeout::integer()} | ignore |
        {stop, Reason :: any()}.
init(Opts) ->
  try
    File    = maps:get(filename,   Opts),
    Vars    = maps:get(vars,       Opts, []),
    OnNew   = maps:get(on_new,     Opts, ""),
    UTC     = maps:get(utc,        Opts, false),
    Rotate  = maps:get(rotate,     Opts, date),
    Keep    = maps:get(keep_files, Opts, 5),
    Pfx     = case maps:get(prefix,Opts) of
                none -> "";
                Str  -> Str ++ ": "
              end,
    is_list(OnNew)
      orelse  is_binary(OnNew)
      orelse  throw({invalid_option, on_new, OnNew}),
    Rotate == date
      andalso string:find(File, "%d") == nomatch
      andalso throw("log filename must contain \"%d\""),
    {ok, #state{fname_mask=File, on_new=OnNew, utc=UTC,
                rotate=Rotate, vars=Vars, keep_files=Keep, prefix=Pfx}}
  catch _:Err:ST ->
    ?LOG_ERROR(#{info => "Error starting logger", error => Err, stack => ST}),
    {stop, {Err, ST}}
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

handle_cast(Other, #state{prefix=Pfx} = State) ->
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
handle_info(Msg, #state{prefix=Pfx} = State) ->
  ?LOG_ERROR("~w: unhandled message: ~p", [Pfx, Msg]),
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

now() -> erlang:system_time(millisecond).

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
  Thr = application:get_env(application:get_application(), async_log_threshold),
  if Len < Thr ->
    gen_server:cast(Pid, Details);
  true ->
    gen_server:call(Pid, Details)
  end.

op(Data) when is_list(Data)       -> log;
op(Data) when is_binary(Data)     -> log;
op(Data) when is_function(Data,0) -> log_fun.

direction(in)        -> in;
direction(out)       -> out;
direction(undefined) -> undefined.

now(undefined)  -> undefined;
now(T) when is_tuple(T) -> T.

dir(in)         -> " <- ";
dir(out)        -> " -> ";
dir(undefined)  -> "".

%%------------------------------------------------------------------------------
%% @doc  Save `Data' to a log file.  Opens the log file if it's not already
%%       open and returns its file descriptor.
%% @end
%%------------------------------------------------------------------------------
-spec log_to_file(#state{}, TimeWithMsec::integer(),
                  in|out|undefined, file:io_data()) ->
        NewState::#state{}.
log_to_file(State, Now, Dir, Data) ->
  case maybe_open_file(State, Now) of
    {true, DateTime, State1} ->
      State2 = safe_write_file(DateTime, header, State#state.on_new, State1),
      safe_write_file(DateTime, Dir, Data, State2);
    {false, DateTime, State1} ->
      safe_write_file(DateTime, Dir, Data, State1)
  end.

maybe_open_file(#state{fd=undefined}=S, Now) ->
  FN = filename(Now, S),
  N  = filelib:file_size(FN),
  case file:open(FN, ?FILE_OPTS) of
  {ok, NewDev} ->
    {{Date,_},_} = Time = split_time(now(), S#state.utc),
    Created = N == 0,
    Created andalso ?LOG_INFO("~screated new log file ~s", [S#state.prefix, FN]),
    {Created, Time, S#state{fd=NewDev, fname=FN, date=Date}};
  {error, Reason} ->
    E = file:format_error(Reason),
    ?LOG_ERROR("~serror opening packet log file ~p: ~s", [S#state.prefix, FN, E]),
    erlang:error(FN ++ ": " ++ E)
  end;
maybe_open_file(State, Now) ->
  check_rotate(State, Now).

check_rotate(#state{fd=Dev, utc=UTC, rotate=Rot} = S, Now) when Dev /= undefined ->
  {{Date,_}, _} = Time = split_time(Now, UTC),
  {S1, Time1, RotType, Rotate} =
    case Rot of
      date when Date /= S#state.date ->
        {S, Time, date, true};
      {size, Size} when Now - S#state.sz_chk > 15000 ->
        N = filelib:file_size(S#state.fname),
        {S#state{sz_chk=Now}, Time, size, N > Size};
      _ ->
        {S, Time, undefined, false}
    end,
  if Rotate ->
    file:close(Dev),
    rotate_files(RotType, S1),
    maybe_open_file(S1#state{fd=undefined, fname=undefined}, Now);
  true ->
    {false, Time1, S1}
  end.

safe_write_file(_DT, header, H, State) when H==""; H == <<"">>; H==undefined ->
  State;
safe_write_file(_DT, header, Data, State) ->
  do_write(State, Data);
safe_write_file(DateTime, Dir, Data, State) ->
  Pfx  = format_time(DateTime),
  DirL = dir(Dir),
  do_write(State, [Pfx, DirL, Data]).

do_write(State = #state{fd=Dev, prefix=Pfx}, Data) ->
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
filename(Now,  #state{fname_mask=Fname, utc=UTC, prefix=Pfx, vars=Bindings}) ->
  {{{Y,M,D},_},_} = split_time(Now, UTC),
  File = replace(Fname, {Y,M,D}, Bindings),
  case filelib:ensure_dir(File) of
    ok ->
      File;
    {error, err} ->
      ?LOG_ERROR("~scannot ensure directory for file ~s: ~p", [Pfx, File, err]),
      erlang:error({log_file, File, err})
  end.

replace(File, {Y,M,D}, Bindings) ->
  File1 = lists:foldl(
    fun({S,I}, A) -> lists:append(string:replace(A, S, i2lp(I), all)) end,
    File,
    [{"%Y", Y}, {"%m", M}, {"%d", D}]),
  env:subst_env_path(File1, Bindings).

-spec time(MSec::integer(), UTC::boolean()) -> list().
time(Now, UTC) ->
  DateTimeWithMsec = split_time(Now, UTC),
  format_time(DateTimeWithMsec).

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

i2b(I) when I < 10    -> <<$0, ($0+I)>>;
i2b(I)                -> integer_to_binary(I).

i2b3(I) when I < 10   -> <<$0, $0, ($0+I)>>;
i2b3(I) when I < 100  -> <<$0, (i2b(I))/binary>>;
i2b3(I) when I < 1000 -> integer_to_binary(I).

i2lp(I) when I < 10   -> [$0, $0+I];
i2lp(I)               -> integer_to_list(I).

rotate_files(RotType, #state{prefix=Pfx, keep_files=Keep, fname_mask=Fname}=S) ->
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
          NewFile = replace("%Y%m%d." ++ OldFile, date(), []),
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
