%%------------------------------------------------------------------------------
%% Author: Serge Aleynikov <saleyn at gmail dot com>
%%
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
%% *** This file is auto-generated, don't modify by hand!!! ***
%%------------------------------------------------------------------------------

-module(fix_codec).
-export([decode/2, decode/3, decode_msg/1, encode/3]).

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
decode(Mode, Bin, Options) ->
  fix_util:decode(Mode, ?FIX_DECODER_MODULE, ?FIX_VARIANT, Bin, Options).

decode(Mode, Bin) ->
  decode(Mode, Bin, []).

%% Decodes a list of [{Key, Value}] pairs to a FIX message.
%% Use after splitting the message with split/1
decode_msg(Msg) when is_list(Msg) ->
  fix_util:decode_msg(?FIX_DECODER_MODULE, Msg).

-spec encode(nif|native, #header{}, {atom(),map()}) -> binary().
encode(Mode, #header{fields = F} = Hdr, {_MsgType,_} = Msg) ->
  Hdr1 = Hdr#header{fields = F#{'BeginString' => ?FIX_BEGIN_STR}},
  fix_util:encode(Mode, ?FIX_ENCODER_MODULE, ?FIX_VARIANT, Hdr1, Msg).

split(Mode, Bin) -> split(Mode, Bin, []).

-spec split(nif|native, binary(), [binary|full]) ->
        [{atom(), binary()|{integer(),integer()}, any(), {integer(),integer()}}].
split(Mode, Bin, Opts) ->
  fix_util:split(Mode, ?FIX_VARIANT, Bin, Opts).