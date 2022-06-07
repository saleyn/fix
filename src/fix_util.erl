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

-export([now/0, timestamp/0, timestamp/1, decode/5, decode_msg/2,
         encode/5, dumpstr/1, dump/1, undump/1]).
-export([try_encode_val/3, try_encode_group/3]).
-export([encode_tagval/2,  encode_tagval/3]).

-compile({no_auto_import,[now/0]}).
-compile({parse_transform,etran}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc fix local reimplementation of UTC as a string
-spec now() -> non_neg_integer().
now() ->
  erlang:system_time(microsecond).

-spec timestamp() -> binary().
timestamp() ->
  fix_nif:encode_timestamp(now()).

-spec timestamp(non_neg_integer() | {calendar:date(), {0..23, 0..59, 0..59, non_neg_integer()}}) ->
  string().
timestamp(EpochUSecs) when is_integer(EpochUSecs) ->
  fix_nif:encode_timestamp(EpochUSecs).

%% @doc Parse the first FIX message in the `Bin' binary.
%% The function returns
%% `{ok, Rest, {MatchedFldCount, Header, Msg, UnparsedFields}}', where
%% `Rest' is the unparsed trailing binary. `MatchedFldCount' is the number
%% of matched fields in the `Msg'. `Header' is the FIX message header.
%% `Msg' is a record containing the FIX message body. `UnparsedFields' are
%% the fields not recognized by the message parser.
-spec decode(nif|native, atom(), atom(), binary(), [binary|full]) ->
  {ok, Rest::binary(), {MatchedFldCount::integer(), Header::#header{},
                        Msg::tuple(), UnparsedFields::list()}}
     | {more, non_neg_integer()}
     | error.
decode(Mode, CodecMod, FixVariant, Bin, Options / []) when is_binary(Bin) ->
  try
    case do_split(Mode, FixVariant, Bin, Options) of
      {ok, MsgLen, Msg} when MsgLen == byte_size(Bin) ->
        {ok, <<>>, decode_msg(CodecMod, Msg)};
      {ok, MsgLen, Msg} ->
        <<_:MsgLen/binary, Rest>> = Bin,
        {ok, Rest, decode_msg(CodecMod, Msg)};
      {error, {_Why, _Pos, _Tag}} = R ->
        R;
      {more, _Size} = R ->
        R
    end
  catch
    error:Error:StackTrace ->
      ?LOG_ERROR(#{info  => "Failed to decode FIX msg",
                   msg   => dump(Bin),
                   error => Error,
                   stack => StackTrace}),
      error(invalid_fix)
  end.

decode_msg(CodecMod, Msg) when is_atom(CodecMod), is_list(Msg) ->
  case CodecMod:decode_msg_header(Msg) of
    {H = #header{fields = #{'MsgType' := MsgType}}, I, L} when I > 0 ->
      {M, I1, U} = CodecMod:decode_msg(MsgType, L),
      {I1, H, M, U};
    _ ->
      false
  end.

-spec encode(nif|native, atom(), atom(), #header{}, {atom(), map()}) -> binary().
encode(Mode, EncoderMod, Variant,
       #header{fields   = #{
        'BeginString'  := FixVerStr,
        'MsgSeqNum'    := SeqNum,
        'SenderCompID' := Sender,
        'TargetCompID' := Target
       } = Hdr},
       {MsgName, MsgFields} = _Msg)
->
  L = ['BeginString', 'MsgSeqNum', 'SenderCompID', 'TargetCompID', 'SendingTime'],
  TStamp0 = maps:get('SendingTime', Hdr, undefined),
  TStamp  = nvl(TStamp0, timestamp()),
  MsgFlds = [KV || KV = {_,V} <- maps:to_list(MsgFields), V /= undefined],
  Fields  = lists:foldl(fun
              ({_,undefined},A) -> A;
              (KV,A) -> [KV | A]
            end, MsgFlds, maps:to_list(maps:without(L, Hdr))),
  Body    = [
    {'MsgType',      MsgName},
    {'SenderCompID', Sender},
    {'TargetCompID', Target},
    {'MsgSeqNum',    SeqNum},
    {'SendingTime',  TStamp}
    | Fields],
  encode_msg(Mode, EncoderMod, Variant, Body, FixVerStr).

encode_msg(nif, _EncoderMod, Variant, Body0, FixVerStr) when is_atom(Variant) ->
  Body1   = encode_msg_nif(Variant, Body0),
  BodyLen = iolist_size(Body1),
  Bin     = iolist_to_binary([
    encode_field_nif(Variant, 'BeginString', FixVerStr),
    encode_field_nif(Variant, 'BodyLength',  BodyLen),
    Body1,
    encode_field_nif(Variant, 'CheckSum', <<"000">>)
  ]),
  fix_nif:update_checksum(Bin);

encode_msg(native, EncoderMod, _Variant, Body0, FixVerStr) ->
  Body1   = encode_msg_native(EncoderMod, Body0),
  BodyLen = iolist_size(Body1),
  Body2   = [
    encode_field_native(EncoderMod, 'BeginString', FixVerStr),
    encode_field_native(EncoderMod, 'BodyLength',  BodyLen),
    Body1
  ],
  Bin  = iolist_to_binary(Body2),
  SumI = lists:sum([Char || <<Char>> <= Bin]) rem 256,
  SumB = list_to_binary(io_lib:format("~3..0B", [SumI])),
  <<Bin/binary, "10=", SumB/binary, 1>>.

encode_field_nif(Variant, Tag, Val) when is_atom(Variant), is_atom(Tag) ->
  fix_nif:encode_field_tagvalue(Variant, Tag, Val).

encode_field_native(CodecMod, Tag, Val) when is_atom(CodecMod), is_atom(Tag) ->
  {_Num, _Type, Fun} = CodecMod:field_tag(Tag),
  Fun(Val).

-spec encode_msg_native(atom(), binary() | proplists:proplist()) -> iolist().
encode_msg_native( CodecMod, [{K,V}|T]) when is_atom(K) ->
  [encode_field_native(CodecMod, K, V) | encode_msg_native(CodecMod, T)];
encode_msg_native( CodecMod, [{K,V,Tag,_Pos}|T]) when is_atom(K), is_integer(Tag) ->
  [encode_field_native(CodecMod, K, V) | encode_msg_native(CodecMod, T)];
encode_msg_native(_CodecMod, []) ->
  [];
encode_msg_native(_CodecMod, Packet) when is_binary(Packet) ->
  Packet.

-spec encode_msg_nif(atom(), binary() | proplists:proplist()) -> iolist().
encode_msg_nif( Variant, [{K,V}|T]) when is_atom(K) ->
  [fix_nif:encode_field_tagvalue(Variant, K, V) | encode_msg_nif(Variant, T)];
encode_msg_nif( Variant, [{K,V,Tag,_Pos}|T]) when is_atom(K), is_integer(Tag) ->
  [fix_nif:encode_field_tagvalue(Variant, K, V) | encode_msg_nif(Variant, T)];
encode_msg_nif(_Variant, []) ->
  [];
encode_msg_nif(_Variant, Packet) when is_binary(Packet) ->
  Packet.

-spec dump(binary() | iolist()) -> binary().
dump(Bin) when is_binary(Bin) ->
  binary:replace(Bin, <<1>>, <<"|">>, [global]);
dump(IOList) when is_list(IOList) ->
  dump(iolist_to_binary(IOList)).

-spec undump(binary()) -> binary().
undump(Bin) when is_binary(Bin) ->
  binary:replace(Bin, <<"|">>, <<1>>, [global]).

-spec dumpstr(binary() | iolist()) -> ok.
dumpstr(Encoded) ->
  io:format("~s~n", [binary_to_list(dump(Encoded))]).

do_split(nif,    Variant, Bin, Opts) -> fix_nif:split(Variant, Bin, Opts);
do_split(native, Variant, Bin, Opts) -> fix_native:split(Variant, Bin, Opts).

try_encode_val(ID, bool,   true)                 -> encode_tagval(ID, $Y);
try_encode_val(ID, bool,   false)                -> encode_tagval(ID, $N);
try_encode_val(ID, int,    V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, length, V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, char,   V) when is_integer(V), V >= $!, V =< $~ -> encode_tagval(ID, $V);
try_encode_val(ID, string, V) when is_list(V)    -> encode_tagval(ID, list_to_binary(V));
try_encode_val(ID, string, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, binary, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, datetm, V) when is_integer(V) -> encode_tagval(ID, fix_nif:encode_timestamp(V));
try_encode_val(ID, datetm, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, T,      V) -> erlang:error({cannot_encode_val, ID, T, V}).

try_encode_group(Mod, ID, [#group{fields=[]}|T]) -> try_encode_group(Mod, ID, T);
try_encode_group(Mod, ID, [#group{fields=L}|_] = V) when is_list(L) ->
  N        = length(V),
  DelimFld = element(1, hd(L)),
  [try_encode_val(ID, length, N) | try_encode_group_items(Mod, ID, DelimFld, V)];
try_encode_group(_Mod, ID, []) ->
  encode_tagval(ID, 0).

%% The group fields must all start from the same field name, so that the FIX
%% decoder can decode groups correctly
try_encode_group_items(Mod, ID, DelimFld, [#group{fields=[{DelimFld, _}|_]=V}|T]) ->
  Data = [encode_field(Mod, K, I) || {K,I} <- V],
  [Data | try_encode_group_items(Mod, ID, DelimFld, T)];
try_encode_group_items(_Mod, ID, DelimFld, [#group{fields=[{Other, _}|_]}|_]) ->
  S = str("invalid first tag for group ~w: expected=~w, got=~w", [ID, DelimFld, Other]),
  erlang:error(list_to_binary(S));
try_encode_group_items(_Mod, _ID, _DelimFld, []) ->
  [].

encode_tagval(ID, V)        -> encode_tagval(ID, V, true).
encode_tagval(ID, V, true)  -> [integer_to_binary(ID), $=, V, ?SOH];
encode_tagval(ID, V, false) -> [integer_to_binary(ID), $=, V].

encode_field(Mod, ID, Val) when is_atom(ID) ->
  case Mod:field_tag(ID) of
    {_Num, _Type, false} -> encode_tagval(ID, Val, true);
    {_Num, _Type, Fun}   -> Fun(Val)
  end.


-ifdef(TEST).
-endif.
