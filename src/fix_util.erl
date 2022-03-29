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
         encode/8, dumpstr/1, dump/1, undump/1]).

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

-spec encode(atom(), atom(), tuple(), non_neg_integer(), binary(),
             binary(), non_neg_integer(), binary()) ->
        binary().
encode(Mode, EncoderMod, Msg, SeqNum, Sender, Target, SendingTime, FixVerStr)
  when is_tuple(Msg), is_integer(SeqNum)
     , is_binary(Sender), is_binary(Target)
     , is_integer(SendingTime), is_binary(FixVerStr)
->
  MsgName = element(1, Msg),
  Fields  = element(2, Msg),
  Body0   = [
    {'MsgType',      MsgName},
    {'SenderCompID', Sender},
    {'TargetCompID', Target},
    {'MsgSeqNum',    SeqNum}
    | [KV || KV = {_,V} <- maps:to_list(Fields), V /= undefined]],
  Body1 =
    case maps:get('SendingTime', Fields, undefined) of
      undefined ->
        [encode(EncoderMod, Body0),
         encode_field(EncoderMod, 'SendingTime', timestamp())];
      _ ->
        encode(EncoderMod, Body0)
    end,
  BodyLen = iolist_size(Body1),
  Body2   = [
    encode_field(EncoderMod, 'BeginString', FixVerStr),
    encode_field(EncoderMod, 'BodyLength',  BodyLen),
    Body1
  ],
  update_checksum(Mode, EncoderMod, Body2).

encode_field(CodecMod, Tag, Val) when is_atom(CodecMod), is_atom(Tag) ->
  {_Num, _Type, Fun} = CodecMod:field_tag(Tag),
  Fun(Val).

-spec encode(atom(), binary() | proplists:proplist()) -> iolist().
encode(_CodecMod, Packet) when is_binary(Packet) -> Packet;
encode( CodecMod, [{K,V}|T]) when is_atom(K) ->
  [encode_field(CodecMod, K, V) | encode(CodecMod, T)];
encode( CodecMod, [{K,V,Tag,_Pos}|T]) when is_atom(K), is_integer(Tag) ->
  [encode_field(CodecMod, K, V) | encode(CodecMod, T)];
encode(_Variant, []) ->
  [].

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

-spec update_checksum(nif|native, atom(), [{atom(), any()}]) -> binary().
update_checksum(nif, EncoderMod, Msg) ->
  Body = [Msg, encode_field(EncoderMod, 'CheckSum', <<"000">>)],
  Bin  = iolist_to_binary(Body),
  fix_nif:update_checksum(Bin);
update_checksum(native, _EncoderMod, Bin) ->
  SumI = lists:sum([Char || <<Char>> <= Bin]) rem 256,
  SumB = list_to_binary(io_lib:format("~3..0B", [SumI])),
  <<Bin/binary, "10=", SumB/binary, 1>>.

-ifdef(TEST).
-endif.
