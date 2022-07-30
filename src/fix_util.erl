%%------------------------------------------------------------------------------
%% @doc Utility functions for encoding/decoding FIX messages
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_util).
-include("fix.hrl").

-export([now/0, timestamp/0, timestamp/1, decode/5,
         dumpstr/1, dump/1, undump/1, split/5]).
-export([try_encode_val/3, try_encode_group/3]).
-export([encode_tagval/2,  encode_tagval/3]).

-compile({no_auto_import,[now/0]}).
-compile({parse_transform,etran}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(FIX_SO_FILE_MASK, "fix_variant*.so").

%% @doc fix local reimplementation of UTC as a string
-spec now() -> non_neg_integer().
now() ->
  erlang:system_time(microsecond).

-spec timestamp() -> binary().
timestamp() ->
  fix_nif:encode_timestamp(now()).

-spec timestamp(non_neg_integer()) -> binary().
timestamp(EpochUSecs) when is_integer(EpochUSecs) ->
  fix_nif:encode_timestamp(EpochUSecs).

%% @doc Parse the first FIX message in the `Bin' binary.
%% The function returns
%% `{ok, Rest, {MatchedFldCount, Header, Msg, UnparsedFields}|false}', where
%% `Rest' is the unparsed trailing binary. `MatchedFldCount' is the number
%% of matched fields in the `Msg'. `Header' is the FIX message header.
%% `Msg' is a record containing the FIX message body. `UnparsedFields' are
%% the fields not recognized by the message parser. The 3rd element will be
%% `false' if the codec couldn't parse the message (e.g. the header doesn't
%% contain required fields.
-spec decode(nif|native, atom(), atom(), binary(), [binary|full]) ->
  {ok, Rest::binary(), {MatchedFldCount::integer(), Header::map(),
                        {MsgType::atom(),Msg::map()}, UnparsedFields::list()} |
                       false}
     | {more, non_neg_integer()}
     | error.
decode(Mode, CodecMod, FixVariant, Bin, Options / []) when is_binary(Bin) ->
  try
    case do_split(Mode, CodecMod, FixVariant, Bin, Options) of
      {ok, MsgLen, Msg} when MsgLen == byte_size(Bin) ->
        {ok, <<>>, CodecMod:decode_msg(Msg)};
      {ok, MsgLen, Msg} ->
        <<_:MsgLen/binary, Rest/binary>> = Bin,
        {ok, Rest, CodecMod:decode_msg(Msg)};
      {error, {_Why, _Pos, _Tag}} = R ->
        R;
      {more, _Size} = R ->
        R
    end
  catch
    error : #{'__exception__' := _} = E : ST ->
      erlang:raise(error, E#{bin => Bin}, ST);
    error:Error:ST ->
      erlang:raise(error, {"Failed to decode FIX msg", Error, Bin}, ST)
  end.

-spec dump(binary() | iolist()) -> binary().
dump(Bin) when is_binary(Bin) ->
  binary:replace(Bin, <<1>>, <<"|">>, [global]);
dump(IOList) when is_list(IOList) ->
  dump(iolist_to_binary(IOList)).

-spec undump(binary()) -> binary().
undump(Bin) when is_binary(Bin) ->
  binary:replace(Bin, [<<"|">>,<<$^,$A>>], <<1>>, [global]).

-spec dumpstr(binary() | iolist()) -> ok.
dumpstr(Encoded) ->
  io:format("~s~n", [binary_to_list(dump(Encoded))]).

do_split(nif,   _CodecMod,  Variant, Bin, Opts) -> fix_nif:split(Variant, Bin, Opts);
do_split(native, CodecMod, _Variant, Bin, Opts) -> fix_native:split(CodecMod, Bin, Opts).

split(nif,   _CodecMod, Variant,  Bin, Opts) -> fix_nif:split(Variant, Bin, Opts);
split(native, CodecMod,_Variant, Bin,_Opts)  -> fix_native:split(CodecMod, Bin).

try_encode_val(ID, bool,   true)                 -> encode_tagval(ID, $Y);
try_encode_val(ID, bool,   false)                -> encode_tagval(ID, $N);
try_encode_val(ID, int,    V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, float,  V) when is_float(V)   -> encode_tagval(ID, float_to_binary(V, [{decimals, 10}, compact]));
try_encode_val(ID, float,  V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, float,  V={decimal,_,_})      -> fix_native:to_binary(V), encode_tagval(ID, V);
try_encode_val(ID, length, V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, char,   V) when is_integer(V), V >= $!, V =< $~ -> encode_tagval(ID, $V);
try_encode_val(ID, string, V) when is_list(V)    -> encode_tagval(ID, list_to_binary(V));
try_encode_val(ID, string, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, binary, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, datetm, V) when is_integer(V) -> encode_tagval(ID, fix_nif:encode_timestamp(V));
try_encode_val(ID, datetm, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, T,      V) -> erlang:error({cannot_encode_val, ID, T, V}).

try_encode_group(Mod, ID, [H|_] = V) ->
  N            = integer_to_binary(length(V)),
  {DelimFld,_} = first_element(H),
  Items        = try_encode_next_group(Mod, ID, DelimFld, V),
  [encode_tagval(ID, N) | Items];
try_encode_group(_Mod, ID, []) ->
  encode_tagval(ID, <<"0">>).

first_element(M) when is_map(M) ->
  I = maps:next(maps:iterator(M)),
  {element(1,I), I};
first_element([{K,_}|_]=L) ->
  {K, L}.

%% The group fields must all start from the same field name, so that the FIX
%% decoder can decode groups correctly
try_encode_next_group(Mod, ID, DelimFld, [G|NextG]) ->
  case first_element(G) of
    {DelimFld, Iter} ->
      [encode_group(Mod, Iter) | try_encode_next_group(Mod, ID, DelimFld, NextG)];
    {Other, _} ->
      S = str("invalid first tag for group ~w: expected=~w, got=~w", [ID, DelimFld, Other]),
      erlang:error(list_to_binary(S))
  end;
try_encode_next_group(_Mod, _ID, _DelimFld, []) ->
  [].

encode_group(Mod, {K,V,It}) ->
  [encode_field(Mod, K, V) | encode_group(Mod, It)];
encode_group(Mod, L) when is_list(L) ->
  [encode_field(Mod, K, I) || {K,I} <- L];
encode_group(_, _) ->
  [].

encode_tagval(ID, V)        -> encode_tagval(ID, V, true).
encode_tagval(ID, V, true)  -> [integer_to_binary(ID), $=, V, ?SOH];
encode_tagval(ID, V, false) -> [integer_to_binary(ID), $=, V].

encode_field(Mod, ID, Val) when is_atom(ID) ->
  case Mod:field_tag(ID) of
    {Num,  _Type, false} -> encode_tagval(Num, Val, true);
    {_Num, _Type, Fun}   -> Fun(Val)
  end.

-ifdef(TEST).
-endif.
