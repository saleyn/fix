%%------------------------------------------------------------------------------
%% @doc Native FIX encoding/decoding functions
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_native).

-export([split/2, encode_field/3, decode_field/3, decode_field/4]).

-compile({parse_transform, ct_expand}). %% See parse_trans library

%% @doc Split a FIX binary message
%% `DecoderMod' - implementation module for field decoding
split(DecoderMod, Bin = <<"8=FIX", _/binary>>) ->
  case binary:match(Bin, [<<1>>,<<$|>>]) of
    {I,1} -> split(DecoderMod, Bin, binary:at(Bin, I));
    _     -> {error, {missing_soh, 0, 8}}
  end.

split(DecoderMod, Bin,  1) -> split2(DecoderMod, Bin,  1, ct_expand:term(element(2,re:compile(<<"([0-9]+)=([^\1]+)\\1">>))));
split(DecoderMod, Bin, $|) -> split2(DecoderMod, Bin, $|, ct_expand:term(element(2,re:compile(<<"([0-9]+)=([^|]+)\\|">>)))).

decode_field(DecoderMod, T={_,_}, V={_,_}, Bin) ->
  Tag = binary:part(Bin, T),
  Val = binary:part(Bin, V),
  decode_field2(DecoderMod, Tag, Val, V).

decode_field(DecoderMod, Key, Val) ->
  {Name, V, Tag, _} = decode_field2(DecoderMod, Key, Val, undefined),
  {Name, V, Tag}.

decode_field2(DecoderMod, Key, Val, VP) when is_binary(Key), is_binary(Val) ->
  Tag = binary_to_integer(Key),
  {N, V, Fun} =
    case DecoderMod:field(Tag) of
      {Name, int,    F} -> {Name, binary_to_integer(Val), F};
      {Name, float,  F} -> {Name, try binary_to_float(Val) catch _:_ -> float(binary_to_integer(Val)) end, F};
      {Name, length, F} -> {Name, binary_to_integer(Val), F};
      {Name, string, F} -> {Name, Val, F};
      {Name, binary, F} -> {Name, Val, F};
      {Name, datetm, F} -> {Name, fix_nif:decode_timestamp(Val, utc), F};
      {Name, bool,   F} when Val==<<$Y>>       -> {Name, true , F};
      {Name, bool,   F} when Val==<<$N>>       -> {Name, false, F};
      {Name, char,   F} when byte_size(Val)==1 -> {Name, binary:at(Val, 0), F};
      {Name, group, _F} -> {Name, Val, group}
    end,
  if is_function(Fun, 1) ->
    {N, Fun(Val), Tag, VP};
  Fun == group ->
    {N, binary_to_integer(V), Tag, VP};
  true ->
    {N, V, Tag, VP}
  end.
encode_field(Codec, Tag, Val) when is_atom(Codec), is_atom(Tag) ->
  {_Num, _Type, Fun} = Codec:field_tag(Tag),
  Fun(Val).

split2(DecoderMod, Bin, Delim, Regex) when is_atom(DecoderMod), is_tuple(Regex) ->
  case binary:match(Bin, <<Delim, "9=">>) of
    {I,N} when byte_size(Bin) > I+N+10 ->
      M = I+N,
      try
        %% NOTE: Len includes delimiter
        {Len, Val} = fix_nif:bin_to_integer(Bin, [{offset, M}, {delim, Delim}]),
        Size = M+Len + Val + 8,  %% 9=XXXX|....|10=XXX|
        case Bin of
          <<B:Size/binary, _/binary>> ->
            {match, L} = re:run(B, Regex, [{capture, all_but_first}, global]),
            Fun  =  fun([Tag,V]) -> decode_field(DecoderMod, Tag, V, Bin) end,
            {ok, Size, lists:map(Fun, L)};
          _ ->
            {more, Size - byte_size(Bin)}
        end
      catch _:_ ->
        {more, 25}
      end;    _ ->
      {error, {}}
  end.
