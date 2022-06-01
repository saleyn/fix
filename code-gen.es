#!/usr/bin/env escript
%%! -env ERL_CRASH_DUMP /dev/null +sbtu +A0
%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc FIX specification generation from XML
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2017 Serge Aleynikov. All rights reserved.
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created: 2017-07-05
%%%-----------------------------------------------------------------------------
-module(fix_code_gen).
-author('saleyn@gmail.com').

-mode(compile). % Compile escript
%-mode(debug). % Debug escript

-compile({parse_transform,  etran}).  % Use parse transforms from etran library
-compile({no_auto_import,[size/1]}).  % We are defining own function size/1

%-include_lib("xmerl/include/xmerl.hrl").
-include_lib("util/include/stringx.hrl").

-export([main/1]).
-export([sep/2]).

%-import(iif, [ife/2, ife/3, ifne/2, ifne/3, iif/3, iif/4]).

-record(state, {
  file,
  doc,
  outdir   = ".",
  save     = false,
  quiet    = false,
  debug    = 0,
  schema   = "fix.schema",
  config   = "fix.config",
  elixir   = false,
  cpp_path = "c_src",
  erl_path = "src",
  inc_path = "include",
  version  = "FIX.4.4",
  variants = [],      %% FIX Variant Name
  variant  = "",      %% FIX Variant Name (required argument)
  var_pfx  = "",      %% FIX variant prefix in lower case
  var_sfx  = "",      %% FIX variant suffix in lower case
  gen      = []  :: [erlang | cpp],
  src_dir  = filename:dirname(filename:absname(escript:script_name())),
  cur_dir  = filename:absname(""),  %% Current directory
  base_dir = false,   %% Is this generator invoked in the 'fix' project's dir?
  copyrt,
  copyyr   = integer_to_list(element(1, date()))
}).

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

main(["-h" ++ _]) ->
  usage();
main(["--help" ++ _]) ->
  usage();
main(Args) ->
  State0 = parse(Args, #state{}),
  Schema = State0#state.schema,

  filelib:is_regular(Schema) orelse abort("Schema file " ++ Schema ++ " not found!"),

  lists:foreach(fun(Variant) ->
    State = update(Variant, State0),

    {Header, _Trailer, Messages, AllMsgGrps, Fields, FldMap} =
      read_file(State),

    generate_fields  (Fields,  FldMap,   State),
    generate_parser  (Header,  Messages, AllMsgGrps, FldMap, State),
    generate_messages(Header,  Messages, AllMsgGrps, FldMap, State)
  end, State0#state.variants),

  copy_makefile(State0).

%%------------------------------------------------------------------------------
%% Code generating functions
%%------------------------------------------------------------------------------

-spec generate_fields([{field, Attrs::list(), Vals::list()}],
                      FldMap::#{integer() =>
                        {Name::atom(), Type::atom(), [{Enum::string(), Descr::string()}]}},
                      #state{}) ->
  #{ID::integer() => {Name::atom(), Type::atom(), [{Enum::string(), Descr::string()}]}}.
generate_fields(Fields, FldMap, #state{} = State) ->
  Types  = lists:sort(sets:to_list(sets:from_list([T || {field,A,_} <- Fields, {type,T} <- A]))),
  MapDT  = #{'AMT'                => float
            ,'BOOLEAN'            => bool
            ,'CHAR'               => char
            ,'COUNTRY'            => string
            ,'CURRENCY'           => string
            ,'DATA'               => binary
            ,'DAYOFMONTH'         => int
            ,'EXCHANGE'           => string
            ,'FLOAT'              => float
            ,'INT'                => int
            ,'LENGTH'             => length
            ,'LOCALMKTDATE'       => string
            ,'MONTHYEAR'          => string
            ,'MULTIPLEVALUESTRING'=> string
            ,'NUMINGROUP'         => group
            ,'PERCENTAGE'         => float
            ,'PRICE'              => float
            ,'PRICEOFFSET'        => float
            ,'QTY'                => float
            ,'SEQNUM'             => int
            ,'STRING'             => string
            ,'UTCDATE'            => datetm
            ,'UTCDATEONLY'        => datetm
            ,'UTCTIMEONLY'        => datetm
            ,'UTCTIMESTAMP'       => datetm
          },
  [maps:find(T, MapDT) == error andalso throw("Found unknown field type: " ++ atom_to_list(T))
   || T <- Types],
  FldList      = maps:to_list(FldMap),
  FldMapByName = maps:fold(fun(ID,{Nm,_Tp,_V},A) -> A#{Nm => ID} end, #{}, FldMap),
  MaxID  = lists:foldl(fun({field, AA, _}, I) -> max(I, get_attr(number, AA)) end, 0, Fields),
  MaxLen = lists:foldl(fun({field, AA, _}, I) -> max(I, length(atom_name(get_attr(name, AA),State))) end, 0, Fields),
  Filenm = add_variant_suffix("fix_fields.cpp", State),
  ok     = write_file(cpp, src, State, Filenm, [], [
            "#include \"util.hpp\"\n\n"
            "namespace {\n\n"
            "std::vector<Field> make_all_fields(FixVariant* fvar)\n"
            "{\n"
            "  assert(fvar);\n"
            "  return std::vector<Field>{{\n",
            lists:map(fun(I) ->
              % ID::integer(), FieldName::quoted_string(), FieldType::string(),
              % FieldType::atom(), Vals::list(), LenFldName::quoted_string()
              {ID, Name, Type, RawType, Vals, LenFldName, LenFldID} =
                case maps:find(I, FldMap) of
                  {ok, {N, 'NUMINGROUP' = T, V}} ->
                    NN         = atom_to_list(N),
                    {Nm, LenF} = {NN, NN}, %% {group_name(NN), NN},
                    {I, Nm, atom_to_list(T), T, V, q(LenF), I};
                  {ok, {N, 'DATA' = T, V}} ->
                    NN = atom_to_list(N),
                    {LF,LI} = % The length field name and ID of the data field has this suffix
                      lists:foldl(fun(Sfx, Ac) ->
                        LenFld = NN ++ Sfx,
                        case maps:find(list_to_atom(LenFld), FldMapByName) of
                          {ok, FldID} -> {LenFld, FldID};
                          error       -> Ac
                        end
                      end,
                      {"", undefined},
                      ["Len", "Length"]),
                    LF == "" andalso
                      throw("Cannot find length field '" ++ LF ++ "' for field " ++ qname(NN)),
                    {I, NN, atom_to_list(T), T, V, q(LF), LI};
                  {ok, {N, T, V}} ->
                    {I, atom_to_list(N), atom_to_list(T), T, V, "", 0};
                  error ->
                    {0, "nullptr", "UNDEFINED", undefined, [], "", 0}
                end,
              LID = integer_to_list(ID),
              LE  = iif(Vals==[], 0, lists:max([length(CC) || {CC,  _} <- Vals])),
              LEn = length(integer_to_list(LE)),
              LL  = length(Vals),
              ME  = LE+2,
              MD  = iif(Vals==[], 0, lists:max([length(atom_name(caml_case(DD),State)) || {_,DD} <- Vals])+2),
              NVals = lists:zip(lists:seq(0, LL-1), Vals),
              [
                "    //--- Tag# ", integer_to_list(I), iif(ID==0, "", " "++q(Name)), "\n",
                "    Field{",
                if ID==0 ->
                  "},\n";
                true ->
                  [
                  "\n"
                  "      fvar,\n"
                  "      ", LID, ",\n"
                  "      ", iif(ID==0, "nullptr", q(Name)), ",\n"
                  "      FieldType::", Type, ",\n"
                  "      ", dtype(RawType, MapDT), ",\n"
                  "      std::vector<FieldChoice>", iif(Vals==[], "(),", "{{"), "\n",
                  lists:map(fun({II,{CC,DD}}) ->
                    io_lib:format(
                  "        {.value = ~s, .descr = ~s, .atom = 0}, // ~w\n",
                                  [qpad(CC,ME), qpad(atom_name(caml_case(DD),State), MD), II])
                  end, NVals),
                  iif(Vals==[], "", "      }},\n"),
                  "      ", iif(LenFldName==[], "nullptr", LenFldName), ",\n"
                  "      ", integer_to_list(LenFldID), ",\n",
                  if Vals==[] ->
                    "      nullptr";
                  true ->
                    "      [](const Field& f, ErlNifEnv* env, const char* code, int len) {"
                  end,
                  if
                    Vals==[] ->
                      "\n";
                    LE == 1 ->
                      ["\n"
                       "        switch (code[0]) {\n",
                       [io_lib:format("          case '~s': return f.value_atom(~s); // ~s\n",
                                      [CC,spad(integer_to_list(II),LEn),DD])
                        || {II,{CC,DD}} <- NVals],
                       "          default: return am_undefined;\n"
                       "        }\n"
                      ];
                    LE > 8 ->
                      ["\n"
                       "        auto   fc = f.value(std::string_view(code, len));\n",
                       "        return fc ? fc->get_atom(env) : am_undefined;\n"
                      ];
                    true ->
                      MML = integer_to_list(lists:max([length(to_char_str(CC)) || {CC,_} <- Vals])),
                      NW  = integer_to_list(length(integer_to_list(length(Vals)))),
                      ["\n"
                       "        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);\n"
                       "        switch (hash) {\n",
                       [io_lib:format("          case ~"++MML++"s: return f.value_atom(~"++NW++"w); // ~s\n",
                          [iif(length(CC)==1, qname(CC), to_char_str(CC)),II,caml_case(DD)])
                        || {II,{CC,DD}} <- NVals],
                       "          default: return am_undefined;\n"
                       "        }\n"
                      ]
                  end,
                  iif(Vals==[], "", "      },\n"),
                  "    },\n"
                  ]
                end
              ]
            end, lists:seq(0, MaxID)),
            "  }};\n"
            "}\n\n"
            "} // namespace\n\n",
            sep(cpp, 0),
            "// Main FIX variant creation function exported from the shared object\n",
            sep(cpp, 0),
            "extern \"C\" {\n"
            "  std::vector<Field> create_fix_fields(FixVariant* fvar)\n"
            "  {\n"
            "    return make_all_fields(fvar);\n"
            "  }\n\n"
            "  const char* get_fix_variant_name() { return \"", get_fix_variant(State), "\"; }\n"
            "}\n\n"
           ]),
  %% Generate fix_fields.erl
  MID = length(integer_to_list(lists:max([ID || {ID, _} <- FldList]))),
  FN2 = add_variant_suffix("fix_fields", State),
  ok  = write_file(erlang, src, State, FN2++".erl", [], [
    "-module(", FN2, ").\n"
    "-export([field/1, field_tag/1]).\n\n"
    "-define(SOH, 1).\n\n",
    lists:map(fun({ID, {Name, Type, Vals}}) ->
      [
        "field(",string:pad(integer_to_list(ID),MID, leading),") -> {",
        string:pad(qname(Name,State),MaxLen+2), ", ",
        string:pad(atom_to_list(maps:get(Type, MapDT)),6),
        ", ",
        iif(Vals==[], "false", ["fun(V) -> decode_fld_val",integer_to_list(ID),"(V) end"]),
        "};\n"
      ]
    end, lists:sort(FldList)),
    "field(", string:pad("_", MID), ") -> false.\n\n",
    [begin
      TT = maps:get(Type, MapDT),
      SS = [string:pad(integer_to_list(ID), MID, leading), ", ", string:pad(atom_to_list(TT),6)],
      ["field_tag(", sqpad(atom_to_list(Name), MaxLen), ") -> {", SS, ", fun(V) -> ",
       if
        TT == group ->
          ["try_encode_group",string:pad(" ",MID-2),"(", string:pad(integer_to_list(ID), MID+8, leading), ", V)"];
        Vals==[] ->
          ["try_encode_val",string:pad(" ",MID),"(", SS, ", V)"];
        true ->
          ["encode_fld_val",string:pad(integer_to_list(ID),MID),"(", SS, ", V)"]
       end,
       " end};\n"
      ]
     end || {ID, {Name, Type, Vals}} <- FldList],
    "field_tag(_) -> erlang:error(badarg).\n\n",
    [begin
      Tp = maps:get(Type, MapDT),
      LE = lists:max([length(CC1) || {CC1,_} <- Vals]),
      ME = LE+2,
      IL = integer_to_list(ID),
      MD = lists:max([length(atom_name(caml_case(DD1),State)) || {_,DD1} <- Vals])+2,
      ["\ndecode_fld_val", IL, "(Val) ->\n",
       "  case Val of",
       [begin
          MD = lists:max([length(atom_name(caml_case(DD1),State)) || {_,DD1} <- Vals])+2,
          io_lib:format("\n    <<~s>> -> ~s; %% ~w",
                        [qpad(CC,ME), string:pad(qname(caml_case(DD),State), MD), II])
        end || {II,{CC,DD}} <- lists:zip(lists:seq(0, length(Vals)-1), Vals)],
       "\n    ", string:pad("_", ME+4), " -> Val"
       "\n  end.\n\n",
       if Tp /= group ->
        [[["encode_fld_val", IL, "(ID,_T, ", sqpad(caml_case(DD), MD), ") -> encode_tagval(ID, <<",
          qpad(CC,ME), ">>);\n"]
          || {CC,DD} <- Vals],
         "encode_fld_val", IL, "(ID, T, ", string:pad("V",MD), ") -> try_encode_val(ID, T, V).\n"];
       true ->
        []
       end
      ]
    end || {ID, {_Name, Type, Vals}} <- lists:sort(FldList), Vals /= []],
    "\n"
    "try_encode_val(ID, bool,   true)                 -> encode_tagval(ID, <<\"Y\">>);\n"
    "try_encode_val(ID, bool,   false)                -> encode_tagval(ID, <<\"N\">>);\n"
    "try_encode_val(ID, int,    V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));\n"
    "try_encode_val(ID, length, V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));\n"
    "try_encode_val(ID, char,   V) when is_integer(V), V >= $!, V =< $~ -> encode_tagval(ID, <<V>>);\n"
    "try_encode_val(ID, string, V) when is_list(V)    -> encode_tagval(ID, list_to_binary(V));\n"
    "try_encode_val(ID, string, V) when is_binary(V)  -> encode_tagval(ID, V);\n"
    "try_encode_val(ID, binary, V) when is_binary(V)  -> encode_tagval(ID, V);\n"
    "try_encode_val(ID, datetm, V) when is_integer(V) -> encode_tagval(ID, fix_nif:encode_timestamp(V));\n"
    "try_encode_val(ID, datetm, V) when is_binary(V)  -> encode_tagval(ID, V);\n"
    "try_encode_val(ID, T,      V) -> erlang:error({cannot_encode_val, ID, T, V}).\n\n"
    "try_encode_group(ID, [{group, _, _M}|_] = V) when is_map(_M) ->\n"
    "  N = length(V),\n"
    "  encode_tagval(ID, [integer_to_binary(N), ?SOH,\n"
    "                      [encode_field(K,I) || {group, _, M} <- V, {K,I} <- maps:to_list(M)]], false);\n"
    "try_encode_group(ID, []) ->\n"
    "  encode_tagval(ID, <<\"0\">>).\n\n"
    "encode_tagval(ID, V) ->\n"
    "  encode_tagval(ID, V, true).\n"
    "encode_tagval(ID, V, true) ->\n"
    "  [integer_to_binary(ID), $=, V, ?SOH];\n\n"
    "encode_tagval(ID, V, false) ->\n"
    "  [integer_to_binary(ID), $=, V].\n\n"
    "encode_field(Tag, Val) when is_atom(Tag) ->\n"
    "  {_Num, _Type, Fun} = field_tag(Tag),\n"
    "  Fun(Val).\n\n"
  ]),
  ok = write_file(erlang, src, State, "fix_codec" ++ State#state.var_sfx ++ ".erl", [], [
    "-module(fix_codec", State#state.var_sfx, ").\n"
    "-export([decode/2, decode/3, decode_msg/1, encode/5, encode/6, encode/7]).\n"
    "\n"
    "-include(\"fix.hrl\").\n"
    "\n"
    "-define(FIX_VARIANT,         ", nvl(State#state.variant, "default"), ").\n"
    "-define(FIX_DECODER_MODULE,  fix_decoder", State#state.var_sfx, ").\n"
    "-define(FIX_ENCODER_MODULE,  fix_fields", State#state.var_sfx, ").\n"
    "-define(FIX_BEGIN_STR,       <<\"", State#state.version, "\">>).\n"
    "\n"
    "%% @doc Parse the first FIX message in the Bin.\n"
    "%% The function returns\n"
    "%% `{ok, BinRest, {MatchedFldCount, Header, Msg, UnparsedFields}}', where\n"
    "%% `BinRest' is the unparsed trailing binary. `MatchedFldCount' is the number\n"
    "%% of parsed fields in the `Msg'. `Header' is the FIX message header.\n"
    "%% `Msg' is a record containing the FIX message body. `UnparsedFields' are\n"
    "%% the fields not recognized by the message parser.\n"
    "-spec decode(nif|native, binary(), [binary|full]) ->\n"
    "  {ok, Rest::binary(), {MatchedFldCount::integer(), Header::#header{},\n"
    "                        Msg::tuple(), UnparsedFields::list()}}\n"
    "    | {more, non_neg_integer()}\n"
    "    | error.\n"
    "decode(Mode, Bin, Options) ->\n"
    "  fix_util:decode(Mode, ?FIX_DECODER_MODULE, ?FIX_VARIANT, Bin, Options).\n"
    "\n"
    "decode(Mode, Bin) ->\n"
    "  decode(Mode, Bin, []).\n"
    "\n"
    "decode_msg(Msg) when is_list(Msg) ->\n"
    "  fix_util:decode_msg(?FIX_DECODER_MODULE, Msg).\n"
    "\n"
    "encode(Mode, Msg, SeqNum, Sender, Target) ->\n"
    "  encode(Mode, Msg, SeqNum, Sender, Target, fix_util:now()).\n"
    "\n"
    "encode(Mode, Msg, SeqNum, Sender, Target, SendingTime) ->\n"
    "  encode(Mode, Msg, SeqNum, Sender, Target, SendingTime, ?FIX_BEGIN_STR).\n"
    "\n"
    "-spec encode(nif|native, tuple(), non_neg_integer(), binary(), binary(),\n"
    "            non_neg_integer(), binary()) -> binary().\n"
    "encode(Mode, Msg, SeqNum, Sender, Target, SendTime, FixVerStr) ->\n"
    "  fix_util:encode(Mode, ?FIX_ENCODER_MODULE, Msg, SeqNum, Sender, Target, SendTime, FixVerStr).\n"
  ]),
  %%if Variant == "" ->
  %%  ok;
  %%true ->
  %%  %% Generate fix_variant_.erl"
  %%  FN3 = "fix_variant" ++ State#state.var_sfx,
  %%  ok  = write_file(erlang, src, State, FN3++".erl", [], [
  %%    "%% @doc FIX encoder/decoder wrappers of the '", Variant, "' FIX variant\n"
  %%    "\n"
  %%    "-module(", FN3, ").\n"
  %%    "\n"
  %%    "-export([split/1, split/2, tag_to_field/1, field_to_tag/1]).\n"
  %%    "-export([decode_field_value/2, encode_field_value/2, list_field_values/1]).\n"
  %%    "\n"
  %%    "split(Bin)                       -> fix_nif:split(", Variant, ", Bin).\n"
  %%    "\n"
  %%    "split(Bin, Opts)                 -> fix_nif:split(", Variant, ", Bin, Opts).\n"
  %%    "\n"
  %%    "tag_to_field(Field)              -> fix_nif:tag_to_field(", Variant, ", Field).\n"
  %%    "\n"
  %%    "field_to_tag(Field)              -> fix_nif:field_to_tag(", Variant, ", Field).\n"
  %%    "\n"
  %%    "encode_field_value(Field, Value) -> fix_nif:encode_field_value(", Variant, ", Field, Value).\n"
  %%    "\n"
  %%    "decode_field_value(Field, Value) -> fix_nif:decode_field_value(", Variant, ", Field, Value).\n"
  %%    "\n"
  %%    "list_field_values(Field)         -> fix_nif:list_field_values(", Variant, ", Field).\n"
  %%  ])
  %%end,
  FldMap.

generate_parser(Header, Messages, _AllMsgGrps, FldMap, #state{var_sfx=SFX} = State) ->
  {'MsgType', _, MsgTypes} = maps:get(35, FldMap),
  MTW  = lists:max([length(atom_to_list(Msg)) || {Msg, _Type, _Cat, _Fields} <- Messages]),
  FNm  = add_variant_suffix("fix_decoder", State),
  ok   = write_file(erlang, src, State, FNm ++ ".erl", [],
  [
    "-module(", FNm, ").\n"
    "-export([decode_msg/2, decode_msg_header/1]).\n\n"
    "-include(\"fix.hrl\").\n"
    "-include(\"", add_variant_suffix("fix_adm_msgs.hrl", State), "\").\n"
    "-include(\"", add_variant_suffix("fix_app_msgs.hrl", State), "\").\n"
    "\n"
    "-define(MAP_SET(_R, _M, _F, _V), _R#_M{fields = (R#_M.fields)#{_F => _V}}).\n"
    "\n"
    "decode_msg_header(Msg) ->\n"
    "  decode_msg_header(Msg, #header{}, 0, []).\n"
    "\n",
    lists:map(fun({Enum, _Descr}) ->
      case lists:keyfind(Enum, 2, Messages) of
        false ->
          [];
        {Msg, _Type, _Cat, _Fields} ->
          [
            "decode_msg(", sqpad(Msg, MTW+2), ",L)", spad("", MTW),
            " -> ", ["decode_msg_", spad(Msg,MTW), "(L, #", sq(Msg), "{}, 0, []);\n"]
          ]
      end
    end, MsgTypes),
    "decode_msg(_,", spad("_", MTW+2), ")", spad("", MTW), " -> false.\n"
    "\n",
    lists:map(fun({Enum, _Descr}) ->
      try
        {Msg, _Type, _Cat, Fields} =
          case lists:keyfind(Enum, 2, Messages) of
            false when Enum == header ->
              Header;
            false ->
              %io:format(standard_error, "WARN: Ignoring message not found in spec (type='~s'): ~s\n", [Enum, caml_case(Descr)]),
              throw(ignore);
            R ->
              R
          end,
        MWD  = lists:max([length(atom_to_list(M)) || {M, _T, _C, _FF} <- Messages]),
        Flds = [{GrpOrFld, get_attr(name,A)} || {GrpOrFld, A,_F} <- Fields],
        MFW  = lists:max([length(sq(N)) || {_, N} <- Flds]),
        %MGW  = iif(Grps==[], 0, lists:max([length(atom_to_list(group_name(G))) || G <- Grps])),
        MsgName = atom_to_list(Msg),
        [
          lists:map(fun({GrpOrFld, FName}) ->
            IsGrp = GrpOrFld==group,
            GN    = atom_to_list(iif(IsGrp, group_name(FName), FName)),
            QGN   = iif(IsGrp, GN, sq(GN)),
            {Var, Val, Res, Add} =
              case GrpOrFld of
                group -> {"L", "N", "G", [" {G,L} = forgrp(",QGN,", N, T, fun ","fix_groups",SFX,":decode_", MsgName, "_", GN, "/2),"]};
                field -> {"T", "V", "V", []}
              end,
            [
              "decode_msg_", spad(MsgName, MWD), "([{", sqpad(FName,MFW), ", ", Val, ",_Tag,_Pos}|T], R, I, U) ->",
              Add,
              " decode_msg_", MsgName, "(", Var, ", ?MAP_SET(R, ", sq(MsgName), ", ", sqpad(FName,MFW), ", ", Res, "), I+1, U);\n"
            ]
          end, Flds),
          "decode_msg_", spad(MsgName, MWD), "([{K,V}|T], R, I, U) ", spad("", MFW+9), " -> decode_msg_", MsgName, "([{K,V,undefined,undefined}|T], R, I, U);\n",
          if Msg == header ->
            ["decode_msg_", spad(MsgName, MWD), "(L, R, I, _) -> {R, I, L}.\n\n"];
          true ->
            ["decode_msg_", spad(MsgName, MWD), "([H|T], R, I, U) -> decode_msg_", MsgName, "(T, R, I, [H|U]);\n"
             "decode_msg_", spad(MsgName, MWD), "([], R, I, U)    -> {R, I, lists:reverse(U)}.\n\n"]
          end
        ]
      catch _:ignore ->
        []
      end
    end, [{header, ""} | MsgTypes]),
    "\n"
    "forgrp(GN,N, L, F) ->\n"
    "  Init = #group{name = GN, fields = #{}},\n"
    "  forgrp(1, N, F, Init, F(L, Init), []).\n"
    "forgrp(I, I,_F,_Init, {L,R}, Acc) ->\n"
    "  {lists:reverse([R|Acc]), L};\n"
    "forgrp(I, N, F, Init, {L,R}, Acc) ->\n"
    "  forgrp(I+1, N, F, Init, F(L, Init), [R|Acc]).\n"
  ]).

generate_messages(Header, Messages, AllGrps, FldMap, #state{config = Cfg, var_sfx=SFX} = State) ->
  NameMap   = maps:fold(fun(ID, {Name, _Type, _Vals}, A) -> A#{Name => ID} end, #{}, FldMap),
  MaxIDLen  = length(integer_to_list(maps:fold(fun(ID, _, A) -> max(ID,A) end, 0, FldMap))),
  Mandatory = proplists:get_value(mandatory, Cfg, #{}),
  %% Check mandatory message configs for invalid message names
  maps:foreach(fun(Msg, _) ->
    lists:keyfind(Msg, 1, Messages) == error
      andalso throw("Invalid message name ~w found in mandatory message config!")
  end, Mandatory),
  %% Check mandatory message configs for invalid field names
  maps:foreach(fun(Msg, Flds) ->
    [maps:find(I, NameMap) == error
        andalso throw("Invalid mandatory field ~w found in configuration of msg ~w", [I, Msg])
      || I <- Flds]
  end, Mandatory),

  [{header, _, _, _HdrFlds} | Msgs] = AllMsgs =
    lists:map(fun({M,T,C,FF}) ->
      {M, T, C, split_req_and_opt_fields(FF, maps:find(M, Mandatory))}
    end, [Header | Messages]),

  %%GrpNames = [G || {_Msg, G, _FF} <- AllGrps]
  %%         / sets:from_list()
  %%         / sets:to_list(),

  MaxMsgLen =
    lists:max([length(atom_to_list(M)) || {M,_T,_C,_FF} <- AllMsgs]),
  MaxReqFldLen =
    lists:max([length(atom_to_list(get_name(F))) || {_M,_T,_C,FF} <- AllMsgs, {req, R} <- FF, F <- R]),
  MaxNLen =
    lists:max([length(atom_to_list(get_name(F))) || {_M,_T,_C,FF} <- AllMsgs, {_ReqOrOpt, R} <- FF, F <- R]),
  %MaxGrpLen =
  %  lists:max([length(atom_to_list(group_name(G))) || G <- GrpNames]),

  Name2ID  = fun(N) -> maps:get(N, NameMap) end,
  % AdminOut = [create_rec(header, "", HdrFlds, State, MaxReqFldLen, MaxIDLen, Name2ID)
  %             | [create_rec(M, Tp, FF, State, MaxReqFldLen, MaxIDLen, Name2ID) || {M, Tp, admin, FF} <- Msgs]],
  AdminOut = [create_rec(M, Tp, FF, State, MaxReqFldLen, MaxIDLen, Name2ID) || {M, Tp, admin, FF} <- Msgs],
  AppOut   = [create_rec(M, Tp, FF, State, MaxNLen, MaxIDLen, Name2ID)      || {M, Tp, app,   FF} <- Msgs],

  ok = write_file(erlang, inc, State, add_variant_suffix("fix_adm_msgs.hrl", State), ["%% Administrative FIX messages\n"], AdminOut),
  ok = write_file(erlang, inc, State, add_variant_suffix("fix_app_msgs.hrl", State), ["%% Application FIX messages\n"],    AppOut),
  ok = write_file(erlang, src, State, add_variant_suffix("fix_groups.erl",   State), ["%% Metadata about FIX groups\n"],
        [
          "-module(fix_groups", SFX, ").\n\n"
          "-include(\"fix.hrl\").\n\n"
          "-export([\n",
          lists:map(fun({M,G,_}) ->
            ["  decode_", atom_to_list(M), "_", atom_to_list(group_name(G)), "/2,\n"]
          end, AllGrps),
          "  decode_group/3\n",
          "]).\n\n",
          %% lists:map(fun({G, FF}) ->
          %%   II = string:pad(integer_to_list(Name2ID(G)), MaxIDLen, leading),
          %%   [
          %%     "%% Group: ", atom_to_list(group_name(G)), "\n",
          %%     [["is_grp_field(", II, ", ", string:pad(integer_to_list(ID), MaxIDLen, leading),
          %%       ") -> true; %% ", atom_to_list(F), "\n"] || {F, ID, _Req, _IsGrp} <- FF],
          %%     "is_grp_field(", II, ", _", string:pad("", MaxIDLen*2-4, leading), ") -> false;\n\n"
          %%   ]
          %% end, Groups),
          %% "is_grp_field(_, ", string:pad("_", MaxIDLen*2-2), ") -> false.\n\n",
          lists:map(fun({Msg, G, _FF}) ->
            GA = sqpad(G, MaxNLen+2),
            %%GN = spad(group_name(G),  MaxGrpLen),
            MN = sqpad(Msg, MaxMsgLen),
            ["decode_group(", MN, ", ", GA, ", L) -> decode_", atom_to_list(Msg), "_", atom_to_list(group_name(G)), "(L, undefined, #group{name=", sq(G), "});\n"]
          end, AllGrps),
          "decode_group(_, _, _) -> false.\n\n",
          lists:map(fun({Msg, G, FF}) ->
            GN = atom_to_list(group_name(G)),
            FN = atom_to_list(Msg) ++ "_" ++ GN,
            ML = lists:max([length(atom_to_list(F)) || {F, _ID, _Req, _IsGrp} <- FF])+2,
            [
              "%% Parse Group: ", GN, " in message ", atom_to_list(Msg), "\n",
              "decode_",  FN, "(L, R) -> decode_", FN, "(L, undefined, R).\n",
              "decode_",  FN, "([{", spad("Delim",ML+2), ",_,_,_}|_]=L, Delim, R) -> {L, R};\n",
              [[
                "decode_",  FN, "([{", spad(sq(F)++"=H", ML+2), ",V,_,_}|T], Delim, #group{fields=F} = R) ->"
                " decode_", FN, "(T, def(Delim,H), R#group{fields = F#{", sqpad(F,ML), " => V}});\n"
               ] || {F, _ID, _Req, _IsGrp} <- FF],
              "decode_", FN, "(L, _Delim, R) -> {L, R}.\n\n"
            ]
          end, AllGrps),
          "\n"
          "def(undefined,N) -> N;\n"
          "def(Other,   _N) -> Other.\n"
        ]),
  ok.

parse(["-f", Xml | T], S) -> parse(T, S#state{file     = Xml});
parse(["-o", Out | T], S) -> parse(T, S#state{outdir   = Out});
parse(["-c", Cfg | T], S) -> parse(T, S#state{config   = Cfg});
parse(["-cr", CR | T], S) -> parse(T, S#state{copyrt   =  CR});
parse(["-vsn", V | T], S) -> parse(T, S#state{version  =   V});
parse(["-vars",V | T], S) -> parse(T, S#state{variants =   V});
parse(["-var", V | T], S) -> V1 = string:to_lower(V),
                             parse(T, S#state{variant  = V1,
                                              var_pfx  = V1++"_",
                                              var_sfx  = "_"++V1
                                             });
parse(["-cdir",D | T], S) -> parse(T, S#state{cpp_path =   D});
parse(["-edir",D | T], S) -> parse(T, S#state{erl_path =   D});
parse(["-idir",D | T], S) -> parse(T, S#state{inc_path =   D});
parse(["-d", Lvl | T], S) -> try
                                I = list_to_integer(Lvl),
                                parse(T, S#state{debug=I})
                             catch _:_ ->
                                parse([Lvl|T], S#state{debug=1})
                             end;
parse(["-e"      | T], S) -> parse(T, S#state{elixir   = true});
parse(["-ge"     | T], S) -> parse(T, S#state{gen      = lists:umerge(S#state.gen, [erlang])});
parse(["-gc"     | T], S) -> parse(T, S#state{gen      = lists:umerge(S#state.gen, [cpp])});
parse(["-p", V   | T], S) -> true = code:add_patha(V), parse(T, S);
parse(["-q"      | T], S) -> parse(T, S#state{quiet    = true});
parse(["-d"      | T], S) -> parse(T, S#state{debug    = 1});
parse(["-h"      | _],_S) -> usage();
parse(["--help"  | _],_S) -> usage();
parse([Other     | _],_S) -> throw({invalid_option, Other});
parse([],              S) ->
  %% Apply defaults here:
  Vars0 = [I || I <- string:split(os:getenv("FIX_VARIANTS", ""), ";", all), I /= []],
  length(Vars0) > 0 andalso S#state.file /= undefined
    andalso abort("Cannot provide -f option when FIX_VARIANTS environment is set!"),

  Vars      = nvl(S#state.variants, Vars0),
  ScriptDir = S#state.src_dir,
  CurDir    = S#state.cur_dir,

  Schema    = filename:join(ScriptDir, S#state.schema),

  S0 = S#state{base_dir = (ScriptDir == CurDir), schema = Schema},

  %% If not in the source repository don't allow empty variants
  S0#state.variant == ""
    andalso not S0#state.base_dir
    andalso abort("Must specify -var argument for the FIX variant!"),

  S0#state.variant == "default"
    andalso not S0#state.base_dir
    andalso abort("Invalid FIX variant name '~s'", [S0#state.variant]),

  V0 = [S0#state.variant]
     / string:lowercase
     / string:replace(" ", "")
     / lists:flatten,

  V  = nvl(V0, "default"),

  S1 = S0#state{gen = nvl(S#state.gen, [erlang,cpp]), variant = V},
  if
    length(Vars) == 0 andalso V0 /= "" ->
      os:putenv("FIX_XML_" ++ string:to_upper(V0), S#state.file),
      S1#state{variants = [V]};
    length(Vars) == 0 ->
      os:putenv("FIX_XML_" ++ string:to_upper(V0), S#state.file),
      S1#state{variants = [""]};
    true ->
      S1#state{variants = Vars}
  end.

update(Variant, S = #state{config = CfgF}) ->
  File = os:getenv("FIX_XML_"++string:to_upper(Variant), undefined),
  CfgD = filename:dirname(CfgF),
  CfgB = filename:basename(CfgF, ".config"),
  Dir  = filename:dirname(escript:script_name()),
  Cfg  = filename:join(Dir, filename:join(CfgD, CfgB ++ "_" ++ Variant ++ ".config")),

  %% Default config file to fix.config
  C = case filelib:is_regular(CfgF) of
        false -> Cfg;
        true  -> CfgF
      end,
  File  == undefined       andalso abort("Input file -f argument or environment 'FIX_XML_~s' missing!", [Variant]),
  filelib:is_regular(File) orelse abort("Input file"   ++ File   ++ " not found!"),
  filelib:is_regular(C)    orelse abort("Config file " ++ C ++ " not found!"),
  {ok,CC} = try file:consult(C) catch _:Err ->
              abort("Cannot read overrides file ~s: ~p", [C, Err])
            end,
  VEmpty  = Variant=="" orelse Variant=="default",
  Vsn     = proplists:get_value(version, CC, S#state.version),
  S#state{version = Vsn,
          variant = Variant,
          var_pfx = iif(VEmpty, "", string:to_lower(Variant)++"_"),
          var_sfx = iif(VEmpty, "", "_" ++ string:to_lower(Variant)),
          file    = File,
          config  = CC}.

usage() ->
  io:format(standard_error,
    "Generate code from FIX XML specification file\n"
    "Usage: ~s [-f File] [-o OutputDir] [-c ConfigFile.config]\n"
    "     -f InputFile.xml\n"
    "     -o OutputDir        - Output relative directory under RootDir (-r)\n"
    "     -c Config           - Configuration file controlling behavior of the code generator (def: fix.config)\n"
    "     -cr Copyright       - Add copyright text to generated files\n"
    "     -e                  - Use Elixir atom prefixing\n"
    "     -ge                 - Generate Erlang code only\n"
    "     -gc                 - Generate C++ code only\n"
    "     -var  Variant       - FIX Variant name (this is a required argument)\n"
    "     -cdir Path          - Output directory for C++ files    (default: ./c_src)\n"
    "     -edir Path          - Output directory for Erlang files (default: ./src)\n"
    "     -vsn  FixVSN        - Fix version tag (#8), default: \"FIX.4.4\"\n"
    "     -q                  - Quiet (don't print WARNING's)\n"
    "     -d [Level]          - Set debug level (no files get saved)\n"
    "     -h | --help         - This help screen\n"
    "\n"
    "Config file (fix.config):\n"
    "=========================\n"
    "{include_app_messages, ['NewOrderSingle', ...]}.\n"
    "\n"
    , [filename:basename(escript:script_name())]),
  halt(1).

read_file(#state{file = Xml, schema = Schema, config = Config}) ->
  InclMsgs   = proplists:get_value(include_app_messages, Config, []),
  % Read file using the Schema to format value types
  {fix, _,L} = xmltree:file(Xml, Schema),
  Components = [C || {components, _, I} <- L, C <- I],
  [Header]   = [{header,  "", admin, expand_components(I, Components, header)}
                || {header,     _, I} <- L],
  [Trailer]  = [{trailer, "", admin, expand_components(I, Components, trailer)}
                || {trailer,    _, I} <- L],
  Fields     = [F || {fields,   _, I} <- L, F <- I],
  %io:format("Fields:\n~p\n", [Fields]),
  Messages   = [{M, T, C, expand_components(I, Components, M)}
                || {messages,   _, J} <- L,
                   {message,    A, I} <- J,
                   {msgcat,        C} <- A,
                   {msgtype,       T} <- A,
                   {name,          M} <- A,
                C==admin orelse InclMsgs==[] orelse lists:member(M, InclMsgs)],
  Fun        = fun G({_, _, _, FL}, Acc) -> lists:foldl(G, Acc, FL);
                   G({field, A, _}, Acc) -> [get_attr(name,A)|Acc];
                   G({group, A,FL}, Acc) -> lists:foldl(G, [get_attr(name,A)|Acc], FL)
               end,
  AllMsgs    = [Header, Trailer | Messages],
  UsedFields = sets:from_list(lists:foldl(Fun, [], AllMsgs)),

  % Make sure that the names of messages are consistent with what's defined
  % in the field#35 (i.e. description of enum values of this field must match
  % the message names under the "messages" node.
  MsgTypes   = {field, [], [{value,[{enum,T},{description,atom_to_list(M)}],[]}
                            || {M, T, _C, _} <- Messages]},
  Fields2    = lists:reverse(
                lists:foldl(fun({field, A, _} = F, Acc) ->
                  Name = get_attr(name, A),
                  case get_attr(number, A) of
                    35 -> [setelement(2, MsgTypes, A) | Acc];
                    _  ->
                      case sets:is_element(Name, UsedFields) of
                        true  -> [F | Acc];
                        false -> Acc
                      end
                  end
                end, [], Fields)),

  FldMap = lists:foldl(fun({_FldOrGrp, A, V} = F, M) ->
    Name = get_attr(name,  A),
    ID   = get_attr(number,A),
    Type = get_attr(type,  A),
    Name /= undefined orelse throw({unknown_field_name, F}),
    is_integer(ID)    orelse throw({unknown_field_number, Name, ID}),
    M#{ID => {Name, Type, [{get_attr(enum, AA), get_attr(description, AA)} || {value, AA, _} <- V]}}
  end, #{}, Fields2),

  NameMap   = maps:fold(fun(ID, {Name, _Type, _Vals}, A) -> A#{Name => ID} end, #{}, FldMap),
  Name2ID  = fun(N) -> maps:get(N, NameMap) end,
  AllGrps  = get_all_groups([Header | Messages], Name2ID, []),
  %io:format("MsgTypes:\n~p\n", [MsgTypes]),
  {Header, Trailer, Messages, AllGrps, Fields2, FldMap}.

%%%-----------------------------------------------------------------------------
%%% Main code generation function
%%%-----------------------------------------------------------------------------
%generate(#state{doc=Doc, debug=Debug}) ->
%  put(doc,     Doc),
%  put(debug,   Debug),
%  try
%    ok
%  catch
%    _:quit ->
%      ok;
%    _:{print, X} ->
%      io:format("~p\n", [X]);
%    _:Reason:StackTrace ->
%      io:format("Error: ~p\n  ~p\n", [Reason, StackTrace])
%  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

expand_components([], _Components, _Msg) ->
  [];
expand_components([{field, _, _} = H | T], Components, Msg) ->
  [H | expand_components(T, Components, Msg)];
expand_components([{group, A, FF} | T], Components, Msg) ->
  L    = expand_components(FF, Components, Msg),
  [{group, A, L} | expand_components(T, Components, Msg)];
expand_components([{component, A, []} | T], Components, Msg) ->
  Name = get_attr(name, A),
  L0   = [F || {component, AA, I} <- Components, F <- I, lists:member({name,Name}, AA)],
  L0  == [] andalso throw("Component " ++ atom_to_list(Name) ++ " not found in message: " ++ atom_to_list(Msg)),
  L1   = expand_components(L0, Components, Msg),
  L1  ++ expand_components(T,  Components, Msg).

split_req_and_opt_fields(Fields, error) ->
  split_req_and_opt_fields(Fields);
split_req_and_opt_fields(Fields, {ok, MandFields}) ->
  FF = [{FldOrGrp, lists:keystore(required, 1, A,
          {required, iif(lists:member(get_attr(name,A), MandFields), add, get_attr(required,A))}), V}
        || {FldOrGrp, A, V} <- Fields],
  split_req_and_opt_fields(FF).

split_req_and_opt_fields(FF) ->
  Req = [iif(LL==[], N, {N, split_req_and_opt_fields(LL)}) || {_, A, LL} <- FF, {name, N} <- A, {required, R}  <- A, lists:member(R, [true, add])],
  Opt = [iif(LL==[], N, {N, split_req_and_opt_fields(LL)}) || {_, A, LL} <- FF, {name, N} <- A, {required, false} <- A],
  case {Req,Opt} of
    {[],[]} -> [];
    {[], _} -> [{opt, Opt}];
    {_, []} -> [{req, Req}];
    _       -> [{req, Req}, {opt, Opt}]
  end.

get_name(N) when is_atom(N) -> N;
get_name({N,_})             -> N.

group_name(A) when is_list(A), is_tuple(hd(A)) ->
  group_name(get_attr(name, A));
group_name(A) when is_tuple(A) ->
  group_name(element(1, A));
group_name(N) when is_atom(N) ->
  case atom_to_list(N) of
    "No" ++ Name -> list_to_atom("grp"++Name);
    Name         -> list_to_atom("grp"++Name)
  end;
group_name("No"++Rest) ->
  list_to_atom("grp"++Rest);
group_name(Name) when is_list(Name) ->
  list_to_atom("grp"++Name).

-spec get_all_groups(list(), fun((atom()) -> integer()), list()) ->
        [{Msg::atom(),
          GrpName::atom(),
          GrpFlds::[{atom(), integer(), Req::boolean(), IsGrp::boolean()}]}].
get_all_groups([], _Name2ID, Acc) ->
  lists:sort([{M,G,F} || {M,{G,F}} <- lists:reverse(Acc)]);
get_all_groups([{Msg, _Name, _MsgCat, Fields}|T], Name2ID, Acc) ->
  Groups = get_groups(Fields, Name2ID, []),
  get_all_groups(T, Name2ID, [{Msg, G} || G <- Groups] ++ Acc).

get_groups([], _Name2ID, Acc) ->
  lists:sort(lists:reverse(Acc));
get_groups([{field, _, _}|T], Name2ID, Acc) ->
  get_groups(T, Name2ID, Acc);
get_groups([{group, GA, Fields}|T], Name2ID, Acc) ->
  A    = get_attr(name, GA),
  Flds = [case F of
            {field, FA, _} ->
              N = get_attr(name, FA),
              R = get_attr(required, FA),
              {N, Name2ID(N), R, false};
            {group, FA, _} ->
              N = get_attr(name, FA),
              R = get_attr(required, FA),
              {N, Name2ID(N), R, true}
          end
          || F <- Fields],
  Grp  = {A, Flds},
  case [{group, [{name, get_attr(name, FA)}], F} || {group, FA, F} <- Fields] of
    [] -> get_groups(T, Name2ID, [Grp | Acc]);
    GG ->
      InnerGrps = get_groups(GG, Name2ID, []),
      get_groups(T, Name2ID, InnerGrps ++ [Grp | Acc])
  end.

%create_rec(Name, group, MsgFields, _State, Margin, MaxIDLen, Name2ID) ->
%  [
%    "%% FIX Group: ", atom_to_list(Name), "\n"
%    "-record(", qname(Name), ", {\n"
%    "  ", string:pad("fields = []", Margin+2), iif(HasFlds, ",", ""), " %% Optional fields\n",
%    ReqFlds,
%    "}).\n\n"
%  ];
create_rec(Name, MsgTp, MsgFields, State, Margin, MaxIDLen, Name2ID) ->
  Names   = if MsgTp == group ->
              [{Fld, ID, IsGrp} || {Fld, ID, true, IsGrp} <- MsgFields];
            true ->
              [begin N=get_name(F), {N, Name2ID(N), is_tuple(F)} end
                || {req, FF} <- MsgFields, F <- FF]
            end,
  NonReq  = if MsgTp == group ->
              [{Fld, ID, IsGrp} || {Fld, ID, false, IsGrp} <- MsgFields];
            true ->
              [begin N=get_name(F), {N, Name2ID(N), is_tuple(F)} end
                || {opt, FF} <- MsgFields, F <- FF]
            end,
  IDNames = [{atom_to_list(N), IsGrp, integer_to_list(ID)} || {N, ID, IsGrp} <- Names],
  Len     = length(IDNames),
  ReqFlds = [["    ", iif(Idx==1, "  ", ", "),
              string:pad(qname(atom_name(iif(IsGrp, group_name(N),N),State)), Margin+2),
              iif(IsGrp," => #{}      ",
                        case Name == header andalso lists:member(N, ["PossDupFlag", "PossResend"]) of
                          true  -> " => false    ";
                          false -> " => undefined"
                        end),
              " %% Tag# ", string:pad(ID, MaxIDLen, leading),
              iif(IsGrp," (GroupLen: "++N++")", ""), "\n"]
             || {Idx, {N,IsGrp,ID}} <- lists:zip(lists:seq(1,Len), IDNames)],
  [
    iif(MsgTp=="" orelse MsgTp==group, "", "%% Message type: \""++MsgTp++"\"\n"),
    "-record(", qname(atom_name(Name, State)), ", {\n"
    "  fields = #{", iif(Len > 0, "\n", ""),  %% Margin+2), iif(HasFlds and not AsMap, ",", ""),
    %%iif(HasFlds and not AsMap, " %% Optional fields", " %% Map of fields"), "\n",
    ReqFlds,
    iif(Len > 0, "  ", ""), "}\n",
    if NonReq /= [] ->
      ["  %% Optional fields:\n"
       "  %% ================\n",
       [["  %% ", "Tag# ", string:pad(integer_to_list(ID), MaxIDLen, leading), ": ",
        atom_to_list(N), "\n"]
         || {N, ID, _IsGrp} <- NonReq]
      ];
    true ->
      []
    end,
    "}).\n\n"
  ].

%%------------------------------------------------------------------------------
%% Get attributes and children
%%------------------------------------------------------------------------------
get_attr(Attr, Doc) when is_atom(Attr), is_tuple(Doc), tuple_size(Doc)=:=3
                       ; is_atom(Attr), is_list(Doc) ->
  case get_attr(Attr, Doc, undefined) of
    undefined -> throw({attribute_not_found, Attr, Doc});
    Other     -> Other
  end;
get_attr(Attrs, Doc) when is_list(Attrs) ->
  [get_attr(A,Doc) || A <- Attrs].

get_attr(Attr, {Item, Attrs, _}, Default) when is_atom(Attr)
                                             , is_atom(Item), is_list(Attrs) ->
  proplists:get_value(Attr, Attrs, Default);
get_attr(Attr, Attrs, Default) when is_atom(Attr), is_list(Attrs) ->
  proplists:get_value(Attr, Attrs, Default).

%%------------------------------------------------------------------------------
%% Separator
%%------------------------------------------------------------------------------
sep(Type, Level) ->
  sep(Type, Level, 2).
sep(Type, Level, Indent) when is_atom(Type), is_integer(Level), is_integer(Indent) ->
  Skip = Level*Indent,
  string:copies(" ", Skip) ++ comment(Type) ++ string:copies("-", 80-2-Skip) ++ "\n".

q(S) when is_list(S) -> "\""++S++"\"";
q(S) when is_atom(S) -> q(atom_to_list(S)).

sq(S) when is_list(S) -> "'"++S++"'";
sq(S) when is_atom(S) -> sq(atom_to_list(S)).

spad(S, I) when is_atom(S) -> string:pad(atom_to_list(S), I);
spad(S, I) when is_list(S) -> string:pad(S, I).

qpad (S, I) -> string:pad(q(S),  I).
sqpad(S, I) -> string:pad(sq(S), I).

dtype(undefined, _) -> "DataType::UNDEFINED";
dtype(T, M)         -> dtype(maps:get(T, M)).

dtype(float)   -> "DataType::DOUBLE";
dtype(int)     -> "DataType::INT";
dtype(length)  -> "DataType::INT";
dtype(group)   -> "DataType::GROUP";
dtype(bool)    -> "DataType::BOOL";
dtype(char)    -> "DataType::CHAR";
dtype(string)  -> "DataType::STRING";
dtype(binary)  -> "DataType::BINARY";
dtype(datetm)  -> "DataType::DATETIME".

atom_name(N, S)                     when is_atom(N) -> atom_name(atom_to_list(N), S);
atom_name(N, #state{elixir = true}) when is_list(N) -> "Elixir." ++ N;
atom_name(N, _)                     when is_list(N) -> N.

qname(N, #state{} = S) ->
  "'"++atom_name(N,S)++"'".

qname(N) when is_atom(N) -> "'"++atom_to_list(N)++"'";
qname(N) when is_list(N) -> "'"++N++"'".

to_char_str(S) ->
  "CINT<"++string:join([qname([C]) || C <- S], ",")++">".

caml_case(S)   ->
  L0 = [titlecase(I) || I <- string:split(string:to_lower(S), "_", all)],
  put(len, 0),
  L = lists:takewhile(fun(I) -> N = get(len) + length(I), put(len, N), N < 128 end, L0),
  string:join(L, "").

titlecase([])    -> [];
titlecase([H|T]) -> [upcase(H)|T].

upcase(C) when C >= $a, C =< $z -> C-32;
upcase(C)                       -> C.

note(Type, #state{base_dir = IsBaseDir, copyrt = Copyright, copyyr = Year}) ->
  C = comment(Type),
  [
    sep(Type, 0),
    if IsBaseDir ->
      [
        C, " Author: Serge Aleynikov <saleyn at gmail dot com>\n",
        C, "\n",
        C, " The work is derived from Maxim Lapshin's open source work:\n",
        C, " https://github.com/maxlapshin/fix under the same open source MIT\n",
        C, " licensing terms as the original.\n",
        sep(Type, 0)
      ];
    is_list(Copyright) ->
      [
        C, " This file is generated by the open source project:\n",
        C, " https://github.com/saleyn/fix\n",
        C, "\n",
        C, " Copyright (c) ", Year, " ", Copyright, ". All rights reserved.\n",
        sep(Type, 0)
      ];
    true ->
      []
    end,
    C, " *** This file is auto-generated, don't modify by hand!!! ***\n",
    sep(Type, 0), "\n"
  ].

comment(cpp)    -> "//";
comment(erlang) -> "%%".

abort(Fmt, Opts / []) ->
  io:format(standard_error, lists:flatten(["ERROR: ", Fmt, "\n"]), Opts),
  halt(1).

get_fix_variant(#state{variant=""}) -> "default";
get_fix_variant(#state{variant=V})  -> V.

add_variant_suffix(File, #state{variant=V}) when V==""; V=="default" ->
  File;
add_variant_suffix(File, #state{var_sfx=Sfx}) ->
  Ext  = filename:extension(File),
  Base = filename:basename(File, Ext),
  Base ++ Sfx ++ Ext.

write_file(Type, SrcOrInc, #state{outdir=Cwd, gen=Gen} = State, File, Header, Data) ->
  case lists:member(Type, Gen) of
    true ->
      Dir =
        case {Type, SrcOrInc} of
          {erlang, src} -> State#state.erl_path;
          {erlang, inc} -> State#state.inc_path;
          {cpp,    src} -> State#state.cpp_path
        end,
      Path = filename:join(Cwd, Dir),
      ok   = filelib:ensure_dir(Path ++ "/"),
      F    = filename:join(Path, File),
      Out  = [iif(Header==[], [], sep(Type, 0)), Header, note(Type, State), Data],
      io:format("Writing file: ~s\n", [F]),
      ok   = file:write_file(F, Out);
    false ->
      ok
  end.

copy_makefile(#state{src_dir = SrcDir, outdir=Cwd, cpp_path = CppPath}) ->
  IncDir   = filename:join(SrcDir, "c_src"),
  Makefile = filename:join(filename:join(Cwd, CppPath), "Makefile"),
  case filelib:is_regular(Makefile) of
    true  -> ok;
    false ->
      DstFile = filename:join(CppPath, "Makefile"),
      SrcFile = filename:join(IncDir,  "Makefile"),
      io:format("Writing file: ~s\n", [DstFile]),
      {ok,F}  = file:read_file(SrcFile),
      Data    = re:replace(F, "INC_DIR\\s*:=[^\n]+", "INC_DIR := -I" ++ IncDir ++ "\n",
                           [{return, binary}]),
      ok      = file:write_file(Makefile, Data)
  end.
