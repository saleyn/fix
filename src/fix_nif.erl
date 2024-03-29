%%------------------------------------------------------------------------------
%% @doc NIF FIX encoding/decoding functions
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_nif).

-on_load(init_nif/0).

-export([load_variant/1, load_variant/2, unload_variant/1, list_variants/0]).
-export([split/2, split/3]).
-export([tag_to_field/2, field_to_tag/2, field_to_bintag/2, field_meta/2]).
-export([bin_to_integer/1, bin_to_integer/2]).
-export([lookup_field_value/3, decode_field_value/3, decode_field_value/4]).
-export([encode_fields/2, encode_field/3]).
-export([list_field_values/2]).
-export([decode_timestamp/1, decode_timestamp/2]).
-export([encode_timestamp/1, encode_timestamp/2, encode_timestamp/3]).
-export([checksum/1,  update_checksum/1]).
-export([strftime/2,  strftime/3]).
-export([pathftime/2, pathftime/3]).
-export([magnitude/1]).

-include("fix.hrl").
-include_lib("kernel/include/logger.hrl").

-type decoded_message() ::
  list({atom(), integer() | binary() | atom() | {integer(), integer()}, integer(), {integer(), integer()}}).
-type split_message()   :: {ok, integer(), decoded_message()} | {more, integer()} | no_return().
-type decode_options()  :: [
  binary |
  full   |
  {float, binary|string|number|decimal|nil} |
  {time,  binary|string|epoch_usec|epoc_msec|epoch_sec|naive|tuple|none|nil}
].
%% Options for splitting a FIX message
%% <dl>
%% <dt>binary</dt>
%%   <dd>Return string fix fields as binaries. Default: tuple `{Pos,Len}'.</dd>
%% <dt>`full'</dt>
%%   <dd>In addition to header and body FIX fields, include `BeginString',
%%      `BodyLength', and `CheckSum'.</dd>
%% <dt>`{float, FloatFmt}'</dt>
%%   <dd>Format floats as `binary', `string', `number' (float value), `decimal'
%%       (tuple `{decimal, Magnitude::integer(), Precision::integer()}').</dd>
%% <dt>`{time, TimeFmt}'</dt>
%%   <dd>Format time as `binary', `string', `epoch_usec' (microseconds since
%%       epoch), `epoch_msec' (milliseconds since epoch), `epoch_sec' (seconds
%%       since epoch), `naive' (Elixir's NaiveDateTime map), `tuple' (tuple
%%       `{{{Y,M,D},{HH,MM,SS}}, USec}'), `none' (tuple `{Pos,Len}' offset/len
%%       part within the FIX message).</dd>
%% </dl>

-export_type([decoded_message/0, decode_options/0]).

-compile({parse_transform,  etran}).  % Use parse transforms from etran library

init_nif() ->
  %% Get the list of supported FIX variants to load. These should be either
  %% directory names containing `*.so' files, or the full `*.so' file names
  %% or application names in which the `priv' dirs will be searched for
  %% '*.so' files:
  TsType = application:get_env(fix, ts_type, sec),
  Debug  = application:get_env(fix, debug,     0),

  lists:member(TsType, [sec, us, ms]) orelse
    throw("Invalid FIX_TS_TYPE=~s (expected: sec|ms|us)", [TsType]),

  Dbg  = max(list_to_integer(os:getenv("FIX_NIF_DEBUG", "0")), Debug),
  Args = [{debug, Dbg}, {ts_type, TsType}],

  Priv = case code:priv_dir(fix) of
           Dir when is_list(Dir) ->
             Dir;
           _ ->
             filename:dirname(code:which(?MODULE)) ++ "/../priv"
         end,
  Load = erlang:load_nif(filename:join(Priv, ?MODULE_STRING), Args),

  case Load of
    ok -> ok;
    {error, {Reason,Text}} ->
      error("Load fix_nif failed. ~p:~p\n", [Reason, Text])
  end.

%% @doc Parse FIX binary message returning string fields as {Pos,Len} offsets.
-spec split(atom(), binary()) -> decoded_message().
split(_Variant, _Binary) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Parse FIX binary message.
%% If `Options' list has the `binary' atom, then return string fields as binaries.
-spec split(atom(), binary(), Options::decode_options()) -> split_message().
split(_Variant, _Binary, _Options) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec field_meta(atom(), binary()|string()|integer()) -> [{atom(), any()}].
field_meta(_Variant, _Field) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec tag_to_field(atom(), binary()|string()|integer()) -> atom().
tag_to_field(_Variant, _Field) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec field_to_tag(atom(), atom()|binary()) -> integer().
field_to_tag(_Variant, _Field) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec field_to_bintag(atom(), atom()|binary()) -> binary().
field_to_bintag(_Variant, _Field) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec encode_fields(atom(), [{atom()|binary()|string()|integer(), any()}]) -> binary().
encode_fields(_Variant, _TagVals) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec encode_field(atom(), atom()|binary()|string()|integer(), any()) -> binary().
encode_field(_Variant, _Field, _Value) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec lookup_field_value(atom(), atom()|binary()|string()|integer(), atom()) ->
        binary().
lookup_field_value(_Variant, _Field, _Value) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec decode_field_value(atom(), atom()|binary()|string()|integer(), binary()) ->
        atom()|binary().
decode_field_value(Variant, Field, Value) ->
  decode_field_value(Variant, Field, Value, nil).

-spec decode_field_value(atom(), atom()|binary()|string()|integer(), binary(),
        nil|                                       %% Default value decoding
        number|decimal|string|binary|              %% Alternative float decoding
        epoch_usec|epoc_msec|epoch_sec|naive|tuple %% Alternative time decoding
      ) -> any().
decode_field_value(_Variant, _Field, _Value, _Format) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec list_field_values(atom(), atom()|binary()|string()|integer()) ->
        [{binary(), atom()}].
list_field_values(_Variant, _Field) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Load a given FIX variant shared object file.
%% Returns false if the variant name is already loaded.
-spec load_variant(binary()|string()|atom()) -> boolean().
load_variant(SoFile) ->
  load_variant(SoFile, preserve).

%% @doc Load a given FIX variant shared object file.
%% If first argument is an application name than
%% If `Action' is `replace', the variant is replaced when it's already loaded.
%% If `Action' is `preserve', the existing variant is not reloaded.
-spec load_variant(atom()|binary()|string(), replace|preserve) -> boolean().
load_variant(AppName, Action) when is_atom(AppName) ->
  case application:load(AppName) of
    ok -> ok;
    {error, {already_loaded, AppName}} -> ok;
    {error, E} ->
      error("Cannot find ~s application: ~p", [atom_to_list(AppName), E])
  end,
  case code:priv_dir(AppName) of
    Dir when is_list(Dir) ->
      SoNameWild = filename:join(Dir, "fix_variant_*.so"),
      case filelib:wildcard(filename:join(Dir, "fix_variant_*.so")) of
        [SoName] ->
          load_fix_variant(SoName, Action);
        [] ->
          erlang:error("Cannot find " ++ atom_to_list(AppName) ++
                       " variant implementation in " ++ SoNameWild);
        [_|_] ->
          erlang:error("Non unique " ++ atom_to_list(AppName) ++
                       " variant implementation found in " ++ SoNameWild)
      end;
    _ ->
      erlang:error("Application " ++ atom_to_list(AppName) ++ " not found")
  end;
load_variant(SoFile, Action) when is_list(SoFile); is_binary(SoFile) ->
  load_fix_variant(SoFile, Action).

-spec load_fix_variant(binary()|string(), replace|preserve) -> boolean().
load_fix_variant(SoFile, _Action) when is_list(SoFile); is_binary(SoFile) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec unload_variant(atom()) -> boolean().
unload_variant(VariantName) when is_atom(VariantName) ->
  unload_fix_variant(VariantName).

-spec unload_fix_variant(atom()) -> boolean().
unload_fix_variant(_VariantName) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Get all loaded FIX variants
-spec list_variants() -> [#{variant => atom(), app => atom()}].
list_variants() ->
  [#{variant => V, app => A} || {V,A} <- list_fix_variants()].

-spec list_fix_variants() -> [{Variant::atom(), AppName::atom()}].
list_fix_variants() ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec bin_to_integer(binary()) -> {Len::integer(), Value::integer()}.
bin_to_integer(_Value) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec bin_to_integer(binary(), [{delim, char()}|{offset, integer()}]) ->
        {Len::integer(), Value::integer()}.
bin_to_integer(_Value, _Options) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Substitute formatted date/time in a string using C strftime() function.
-spec strftime(Fmt::string() | binary(), integer()) -> string() | binary().
strftime(_Format, _NowSecs) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Substitute formatted date/time in a string using C strftime() function.
-spec strftime(Fmt::string() | binary(), integer(), utc | local) ->
  string() | binary().
strftime(_Format, _NowSecs, _Utc) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Substitute formatted date/time in a path using C strftime() function.
%% If the path begins with `"~"', it'll be replaces with the value of `${HOME}'.
%% All environment variables in the path in the form `${VAR}' will be substituted
%% with evaluated values.
-spec pathftime(Fmt :: string() | binary(), integer()) ->
  string() | binary().
pathftime(_Format, _NowSecs) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Substitute formatted date/time in a path using C strftime() function.
-spec pathftime(Fmt::string() | binary(), NowSecs::integer(), utc | local) ->
  string() | binary().
pathftime(_Format, _NowSecs, _Utc) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Convert a UTC timestamp to integer seconds from UNIX epoch.
%% Timestamp can be in the form "YYYYMMDD-hh:mm:ss[.ttt[ttt]]".  Throws
%% `badarg' on failure.
-spec decode_timestamp(binary()) -> integer().
decode_timestamp(_Value) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Convert timestamp to integer micro-/milli-seconds from UNIX epoch.
%% Timestamp can be in the form "YYYYMMDD-hh:mm:ss[.ttt[ttt]]".  Throws
%% `badarg' on failure.
%%
%% Args:
%% <ul>
%% <li>Value     - Usec or Msec from epoch</li>
%% <li>utc|local - UTC (default) or local time</li>
%% </ul>
-spec decode_timestamp(binary(), utc|local) -> integer().
decode_timestamp(_Value, _UTC) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec encode_timestamp(non_neg_integer()) -> binary().
encode_timestamp(_Value) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec encode_timestamp(non_neg_integer(), utc|local) -> binary().
encode_timestamp(_Value, _UTC) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Convert micro-/milli-/seconds from UNIX epoch to a timestamp binary
%% in the form "YYYYMMDD-hh:mm:ss[.ttt[ttt]]".
%% Throws `badarg' on failure or `memalloc' if cannot allocate memory.
%%
%% Args:
%% <ul>
%% <li>Value      - Sec or Usec or Msec from epoch</li>
%% <li>utc|local  - UTC (default) or local time</li>
%% <li>Resolution - `sec' seconds (default) |
%%                  `us'  microseconds      |
%%                  `ms'  milliseconds
%% </li>
%% </ul>
-spec encode_timestamp(non_neg_integer(), utc|local, sec|us|ms) -> binary().
encode_timestamp(_Value, _UTC, _Resolution) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Calculate checksum of the body of the FIX message.
%% This checksum is suitable for storing in the "10=" field of a message trailer.
-spec checksum(binary()) -> non_neg_integer().
checksum(_Bin) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Convert a floating point number stored in a string to an integer magnitude.
%% The number must be under 256 digits. E.g. "123.456" -> 123456.  Supports positive
%% and negative numbers.
-spec magnitude(string()) -> integer().
magnitude(_Str) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% @doc Performs DESTRUCTIVE update of the binary with FIX checksum.
%% The argument must be a FIX encoded binary with a trailer ending with
%% "10=...\x1".  The function will overwrite the "..." with the checksum.
-spec update_checksum(binary()) -> binary().
update_checksum(_Bin) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

bin_to_integer_test() ->
  ?assertEqual({1,1},  bin_to_integer(<<"1">>)),
  ?assertEqual({2,-1}, bin_to_integer(<<"-1">>)),
  ?assertEqual({13,1234567890123}, bin_to_integer(<<"1234567890123">>)),
  ?assertError(badarg, bin_to_integer(<<"abc">>)),
  ?assertError(badarg, bin_to_integer(<<"1e+2">>)),
  ok.

encode_decode_timestamp_test() ->
  ?assertEqual(1646002459113369, decode_timestamp(<<"20220227-22:54:19.113369">>)),
  ?assertEqual(1646002459113369, decode_timestamp(<<"20220227-22:54:19.113369">>, utc)),
  ?assertEqual(1646020459113369, decode_timestamp(<<"20220227-22:54:19.113369">>, local)),
  ?assertEqual(1646002459113369, decode_timestamp(<<"20220227-17:54:19.113369">>, local)),
  ?assertEqual(<<"20220227-22:54:19.113369">>, encode_timestamp(decode_timestamp(<<"20220227-22:54:19.113369">>), utc, us)),
  ?assertEqual(<<"20220227-22:54:19.113369">>, fix_nif:encode_timestamp(1646002459113369, utc,   us)),
  ?assertEqual(<<"20220227-22:54:19.113">>,    fix_nif:encode_timestamp(1646002459113369, utc,   ms)),
  ?assertEqual(<<"20220227-22:54:19">>,        fix_nif:encode_timestamp(1646002459113369, utc,   sec)),
  ?assertEqual(<<"20220227-17:54:19.113">>,    fix_nif:encode_timestamp(1646002459113369, local, ms)),
  ?assertEqual(<<"20220227-17:54:19">>,        fix_nif:encode_timestamp(1646002459113369, local, sec)),
  ?assertEqual(<<"20220227-17:54:19">>,        fix_nif:encode_timestamp(1646002459113369, local)),
  ok.

strftime_test() ->
  "2001-11-12 13:31:01" = fix_nif:strftime("%Y-%m-%d %H:%M:%S", 1005589861, local),
  "2001-11-12 18:31:01" = fix_nif:strftime("%Y-%m-%d %H:%M:%S", 1005589861, utc),
  "2001-11-12 18:31:01" = fix_nif:strftime("%Y-%m-%d %H:%M:%S", 1005589861).

pathftime_test() ->
  Home = os:getenv("HOME"),
  H1   = Home ++ "/2022-03-01.00:57:03.tmp",
  H2   = Home ++ "/2022-02-28.19:57:03.tmp",
  ?assertEqual(H1, fix_nif:pathftime("~/%Y-%m-%d.%H:%M:%S.tmp", 1646096223)),
  ?assertEqual(H2, fix_nif:pathftime("~/%Y-%m-%d.%H:%M:%S.tmp", 1646096223, local)),
  ok.

magnitude_test() ->
  123456789012345678901234 = fix_nif:magnitude("12345.6789012345678901234"),
  12345                    = fix_nif:magnitude("+12.345"),
  -12345                   = fix_nif:magnitude("-12.345"),
  12345                    = fix_nif:magnitude("12345."),
  12345                    = fix_nif:magnitude("12345").

split_and_checksum_test() ->
  HBeatP    = <<"8=FIX.4.0|9=58|35=0|49=BuySide|56=SellSide|34=5|52=20190605-11:57:29.363|10=999|">>,
  HBeatR    = fix_util:undump(HBeatP),
  ?assertEqual(175, fix_nif:checksum(HBeatP)),
  ?assertEqual(175, fix_nif:checksum(HBeatR)),
  Bn2       = fix_util:dump(fix_nif:update_checksum(HBeatR)),
  %%<<"175">> = binary:part(Bn2, {76,3}),
  %% NOTE: destructive update of the 10=xxx field in the prior call:
  ?assertEqual(HBeatR, <<"8=FIX.4.0",1,"9=58",1,"35=0",1,"49=BuySide",1,"56=SellSide",1,"34=5",1,"52=20190605-11:57:29.363",1,"10=175",1>>),
  ?assertEqual(Bn2,    <<"8=FIX.4.0|9=58|35=0|49=BuySide|56=SellSide|34=5|52=20190605-11:57:29.363|10=175|">>),
  ok.

  % Fix = fix_variant:new(default),

  % ?assertEqual({error,{missing_tag9,0,0}},
  %   Fix:split(<<"8=FIX.4.0|9=58|35=0|49=BuySide|56=SellSide|34=5">>)),
  % ?assertEqual({more, 33},
  %   Fix:split(<<"8=FIX.4.0|9=58|35=0|49=BuySide|56=SellSide|34=5">>, [{delim, $|}])),
  % ?assertEqual({more, 33},
  %   Fix:split(fix_util:undump(<<"8=FIX.4.0|9=58|35=0|49=BuySide|56=SellSide|34=5">>))),

  % ?assertEqual({ok,80,
  %   [ {'BeginString',8,{2,7}},
  %     {'BodyLength',9,58},
  %     {'MsgType',35,'Heartbeat'},
  %     {'SenderCompID',49,{23,7}},
  %     {'TargetCompID',56,{34,8}},
  %     {'MsgSeqNum',34,5},
  %     {'SendingTime',52,{51,21}},
  %     {'CheckSum',10,{76,3}}]},
  %   Fix:split(Bn2, [{delim,$|}])),

  % ?assertEqual({ok,80,
  %   [ {'BeginString',8,{2,7}},
  %     {'BodyLength',9,58},
  %     {'MsgType',35,'Heartbeat'},
  %     {'SenderCompID',49,{23,7}},
  %     {'TargetCompID',56,{34,8}},
  %     {'MsgSeqNum',34,5},
  %     {'SendingTime',52,{51,21}}
  %   ]},
  %   Fix:split(HBeatR)),

  % ?assertEqual({ok,80,
  %   [ {'BeginString',8,<<"FIX.4.0">>},
  %     {'BodyLength',9,58},
  %     {'MsgType',35,'Heartbeat'},
  %     {'SenderCompID',49,<<"BuySide">>},
  %     {'TargetCompID',56,<<"SellSide">>},
  %     {'MsgSeqNum',34,5},
  %     {'SendingTime',52,<<"2019005-11:57:29.363">>},
  %     {'CheckSum',10,<<"175">>}]},
  %   Fix:split(HBeatR, [binary])),

  % BinP = <<"8=FIX.4.4|9=22|35=A|93=8|89=ABCD1234|384=2|372=ABC|"
  %          "385=S|372=EFG|385=R|94=0|10=999|">>,
  % Bin  = fix_util:undump(BinP),

  % ?assertEqual({ok,83,
  %   [ {'BeginString',<<"FIX.4.4">>,8,{2,7}},
  %     {'BodyLength',61,9,{12,2}},
  %     {'MsgType','Logon',35,{18,1}},
  %     {'SignatureLength',8,93,{23,1}},
  %     {'Signature',<<"ABCD1234">>,89,{28,8}},
  %     {'NoMsgTypes',2,384,{41,1}},
  %     {'RefMsgType',<<"ABC">>,372,{47,3}},
  %     {'MsgDirection','Send',385,{55,1}},
  %     {'RefMsgType',<<"EFG">>,372,{61,3}},
  %     {'MsgDirection','Receive',385,{69,1}},
  %     {'EmailType','New',94,{74,1}},
  %     {'CheckSum',<<"226">>,10,{79,3}}]},
  %   Fix:split(<<"8=FIX.4.4|9=61|35=A|93=8|89=ABCD1234|384=2|372=ABC|385=S|372=EFG|385=R|94=0|10=226|">>,
  %                      [binary, full])),
  % ok.

% tag_to_field_test() ->
%   Fix = fix_variant:new(default),
%   ?assertEqual('CheckSum', Fix:tag_to_field(10)),
%   ?assertEqual('CheckSum', Fix:tag_to_field("10")),
%   ?assertEqual('CheckSum', Fix:tag_to_field(<<"10">>)),
%   ok.

-endif.
