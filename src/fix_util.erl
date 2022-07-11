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
-export([find_variants/0, add_variant_env/1]).

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

%% @doc Add variant name to `fix_variants' config option of the `fix' app
add_variant_env(Variant) ->
  case application:get_env(fix, fix_variants, []) of
    [] -> application:set_env(fix, fix_variants, [Variant]);
    L  ->
      case lists:member(Variant, L) of
        true  -> ok;
        false -> application:set_env(fix, fix_variants, [Variant|L])
      end
  end.

%% @doc Find all known FIX variants `*.so' NIF libraries.
-spec find_variants() -> [string()].
find_variants() ->
  Priv = code:priv_dir(fix),
  is_list(Priv) orelse
    error("Cannot locate priv directory of the 'fix' application"),
  Root = filename:dirname(filename:dirname(Priv)),

  %% Get the list of supported FIX variants to load. These should be either
  %% directory names containing `*.so' files, or the full `*.so' file names
  %% or application names in which the `priv' dirs will be searched for
  %% '*.so' files:
  Apps  = application:which_applications(),
  Vars  = case application:get_env(fix, fix_variants, []) of
            [] ->
              case get_variants_from_env() of
                [] ->
                  [filename:join(filename:dirname(I), "priv/fix_fields*.so")
                    || I <- code:get_path(), string:find(I, "/fix") /= nomatch];
                VV -> VV
              end;
            Variants ->
              Variants
          end,
  Res = lists:foldl(fun(V, S) ->
    {NoWarning, Msk} =
      if is_atom(V) ->
        case lists:keymember(V, 1, Apps) of
          true ->
            case code:priv_dir(V) of
              {error, _} ->
                error("Undefined priv directory of application: ~w", [V]);
              Dir ->
                {false, filename:join(Dir, "fix_fields*.so")}
            end;
          false ->
            Dir = filename:join([Root, atom_to_list(V), "priv"]),
            case filelib:is_dir(Dir) of
              true  -> {false, filename:join(Dir, "fix_fields*.so")};
              false -> error("Fix variant application '~w' is not known!", [V])
            end
        end;
      is_list(V); is_binary(V) ->
        IsDir   = filelib:is_dir(V),
        IsFile  = filelib:is_file(V),
        Val     = iif(is_binary(V), binary_to_list(V), V),
        IsWild  = string:find(Val, "fix_fields*.so") /= nomatch,
        if IsDir ->
          {false, filename:join(Val, "fix_fields*.so")};
        IsFile; IsWild ->
          {IsWild, Val};
        true ->
          error("File/Dir name '~s' not found!", [Val])
        end;
      true ->
        error("Invalid argument in fix.fix_variants: ~p", [V])
      end,
    case filelib:wildcard(Msk) of
      [] when not NoWarning ->
        logger:warning("No shared object files found matching name: ~s", [Msk]),
        S;
      [] ->
        S;
      Names ->
        Names ++ S
    end
  end, [], Vars),
  lists:usort(Res).

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

get_variants_from_env() ->
  case os:getenv("FIX_VARIANTS") of
    false ->
      [];
    Env ->
      lists:foldl(fun(S,L) ->
        L = [atom_to_list(element(1,A)) || A <- application:which_applications()],
        case lists:member(S, L) of
          true  -> [list_to_atom(S) | L];
          false -> L
        end
      end, [], string:split(Env, ":", all))
  end.

-ifdef(TEST).
-endif.
