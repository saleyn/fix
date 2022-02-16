%%------------------------------------------------------------------------------
%% @doc FIX codec implementation
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_codec).
-export([decode/3, decode_msg/1, encode/5, encode/6, encode/7]).

-compile({parse_transform, etran}).  % Use parse transforms from etran library

-include("fix.hrl").

-define(FIX_VARIANT,         default).
-define(FIX_DECODER_MODULE,  fix_decoder).
-define(FIX_ENCODER_MODULE,  fix_fields).
-define(FIX_BEGIN_STR,       <<"FIX.4.4">>).

%% @doc Parse the first FIX message in the Bin.
%% The function returns
%% `{ok, BinRest, {MatchedFldCount, Header, Msg, UnparsedFields}}', where
%% `BinRest' is the unparsed trailing binary. `MatchedFldCount' is the number
%% of parsed fields in the `Msg'. `Header' is the FIX message header.
%% `Msg' is a record containing the FIX message body. `UnparsedFields' are
%% the fields not recognized by the message parser.
-spec decode(nif|native, binary(), [binary|full]) ->
  {ok, Rest::binary(), {MatchedFldCount::integer(), Header::#header{},
                        Msg::tuple(), UnparsedFields::list()}}
     | {more, non_neg_integer()}
     | error.
decode(Mode, Bin, Options / []) ->
  fix_util:decode(Mode, ?FIX_DECODER_MODULE, ?FIX_VARIANT, Bin, Options).

decode_msg(Msg) when is_list(Msg) ->
  fix_util:decode_msg(?FIX_DECODER_MODULE, Msg).

encode(Mode, Msg, SeqNum, Sender, Target) ->
  encode(Mode, Msg, SeqNum, Sender, Target, fix_util:now()).

encode(Mode, Msg, SeqNum, Sender, Target, SendingTime) ->
  encode(Mode, Msg, SeqNum, Sender, Target, SendingTime, ?FIX_BEGIN_STR).

-spec encode(nif|native, tuple(), non_neg_integer(), binary(), binary(),
             non_neg_integer(), binary()) -> binary().
encode(Mode, Msg, SeqNum, Sender, Target, SendTime, FixVerStr) ->
  fix_util:encode(Mode, ?FIX_ENCODER_MODULE, Msg, SeqNum, Sender, Target, SendTime, FixVerStr).


%encode_field(Key, Val) ->
