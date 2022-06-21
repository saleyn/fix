%%------------------------------------------------------------------------------
%% @doc Native FIX encoding/decoding functions
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_native).

-export([split/2, split/3, encode_field/3]).
-export([decode_field/3, decode_field/4, decode_field/5]).
-export([to_decimal/1, to_binary/1]).

-compile({parse_transform, ct_expand}). %% See parse_trans library

split(DecoderMod, Bin) -> split(DecoderMod, Bin, []).

%% @doc Split a FIX binary message
%% `DecoderMod' - implementation module for field decoding
split(DecoderMod, Bin = <<"8=FIX", _/binary>>, Opts) ->
  case binary:match(Bin, [<<1>>,<<$|>>]) of
    {I,1} ->
      IsFull = lists:member(full, Opts),
      Float  = proplists:get_value(float, Opts, float),
      case split2(DecoderMod, Bin, binary:at(Bin, I), Float) of
        Res = {ok, _Len, _Msgs} when IsFull ->
          Res;
        {ok, Len, Msg} ->
          {ok, Len, [F || F <- Msg, filter_field(element(1, F))]}
      end;
    _ ->
      {error, {missing_soh, 0, 8}}
  end.

split2(DecoderMod, Bin,  1, FloatAs) ->
  Rex = ct_expand:term(element(2,re:compile(<<"([0-9]+)=([^\1]+)\1">>))),
  split3(DecoderMod, Bin,  1, Rex, FloatAs);
split2(DecoderMod, Bin, $|, FloatAs) ->
  Rex = ct_expand:term(element(2,re:compile(<<"([0-9]+)=([^|]+)\\|">>))),
  split3(DecoderMod, Bin, $|, Rex, FloatAs).

decode_field(DecoderMod, Tag, Value, Bin) ->
  decode_field(DecoderMod, Tag, Value, Bin, binary).

decode_field(DecoderMod, T={_,_}, V={_,_}, Bin, FloatAs) ->
  Tag = binary:part(Bin, T),
  Val = binary:part(Bin, V),
  decode_field2(DecoderMod, Tag, Val, FloatAs, V).

decode_field(DecoderMod, Key, Val) ->
  {Name, V, Tag, _} = decode_field2(DecoderMod, Key, Val, binary, undefined),
  {Name, V, Tag}.

decode_field2(DecoderMod, Key, Val, FloatAs, VP) when is_binary(Key), is_binary(Val) ->
  Tag = binary_to_integer(Key),
  {N, V, Fun} =
    try
      case DecoderMod:field(Tag) of
        {Name, int,    F} -> {Name, binary_to_integer(Val), F};
        {Name, float,  F} ->
          VV =
            case FloatAs of
              binary  -> Val;
              decimal -> to_decimal(Val);
              float   -> try binary_to_float(Val) catch _:_ -> float(binary_to_integer(Val)) end
            end,
          {Name, VV, F};
        {Name, length, F} -> {Name, binary_to_integer(Val), F};
        {Name, string, F} -> {Name, Val, F};
        {Name, binary, F} -> {Name, Val, F};
        {Name, datetm, F} -> {Name, fix_nif:decode_timestamp(Val, utc), F};
        {Name, bool,   F} when Val==<<$Y>>       -> {Name, true , F};
        {Name, bool,   F} when Val==<<$N>>       -> {Name, false, F};
        {Name, char,   F} when byte_size(Val)==1 -> {Name, binary:at(Val, 0), F};
        {Name, group, _F} -> {Name, Val, group}
      end
    catch C:E:ST ->
      Err = lists:flatten(io_lib:format(
              "Error decoding FIX field tag=~w, val=~s: ~p", [Tag, Val, E])),
      erlang:raise(C, Err, ST)
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

filter_field('Elixir.BeginString') -> false;
filter_field('BeginString')        -> false;
filter_field('Elixir.BodyLength')  -> false;
filter_field('BodyLength')         -> false;
filter_field('Elixir.CheckSum')    -> false;
filter_field('CheckSum')           -> false;
filter_field(_)                    -> true.

split3(DecoderMod, Bin, Delim, Rex, FloatAs) when is_atom(DecoderMod), is_tuple(Rex) ->
  case binary:match(Bin, <<Delim, "9=">>) of
    {I,N} when byte_size(Bin) > I+N+10 ->
      M = I+N,
      %% NOTE: Len includes delimiter
      {Len, Val} = fix_nif:bin_to_integer(Bin, [{offset, M}, {delim, Delim}]),
      Size = M+Len + Val + 8,  %% 9=XXXX|....|10=XXX|
      case Bin of
        <<B:Size/binary, _/binary>> ->
          {match, L} = re:run(B, Rex, [{capture, all_but_first}, global]),
          Fun  =  fun([Tag,V]) -> decode_field(DecoderMod, Tag, V, Bin, FloatAs) end,
          {ok, Size, lists:map(Fun, L)};
        _ ->
          {more, Size - byte_size(Bin)}
      end;
    _ ->
      {more, 25}
  end.

to_binary({decimal, Mant, Prec}) when is_integer(Mant), is_integer(Prec) ->
  I = pow10(Prec),
  M = Mant div I,
  R = Mant - M*I,
  B = integer_to_binary(M),
  if R == 0 ->
    B;
  true ->
    B2 = integer_to_binary(R),
    <<B/binary, $., B2/binary>>
  end.

to_decimal(V) when is_binary(V) ->
  case binary:split(V, <<".">>) of
    [Int, Frac] ->
      Prec = byte_size(Frac),
      {decimal, binary_to_integer(Int) * pow10(Prec) + binary_to_integer(Frac), Prec};
    [Int] ->
      {decimal, binary_to_integer(Int), 0}
  end.

pow10(0) -> 1;
pow10(I) -> 10*pow10(I-1).

%split3(DecoderMod, Bin, Delim, FloatAs) when is_atom(DecoderMod) ->
%  case binary:match(Bin, <<Delim, "9=">>) of
%    {I,N} when byte_size(Bin) > I+N+10 ->
%      M = I+N,
%      %% NOTE: Len includes delimiter
%      {Len, Val} = fix_nif:bin_to_integer(Bin, [{offset, M}, {delim, Delim}]),
%      Size = M+Len + Val + 8,  %% 9=XXXX|....|10=XXX|
%      case Bin of
%        <<B:Size/binary, _/binary>> ->
%          L = [binary:split(KV, <<"=">>)
%                || KV <- binary:split(B, <<Delim>>, [global, trim])],
%          Fun  =  fun([Tag,V]) -> decode_field(DecoderMod, Tag, V, FloatAs) end,
%          {ok, Size, lists:map(Fun, L)};
%        _ ->
%          {more, Size - byte_size(Bin)}
%      end;
%    _ ->
%      {more, 25}
%  end.