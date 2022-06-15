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
  app_descr= "",
  version,
  variants = [],      %% FIX Variant Name
  file_sfx,
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
    State1 = update(Variant, State0),

    {FixVsn, Header, _Trailer, Messages, AllMsgGrps, Fields, FldMap} =
      read_file(State1),

    State = case State1#state.version of
              undefined -> State1#state{version = FixVsn};
              _         -> State1
            end,

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
generate_fields(Fields, FldMap, #state{var_sfx=SFX} = State) ->
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
  FldMapByName = maps:fold(fun(ID,{Nm,_Tp,_IsGrp,_V},A) -> A#{Nm => ID} end, #{}, FldMap),
  MaxID  = lists:foldl(fun({field, AA, _}, I) -> max(I, get_attr(number, AA)) end, 0, Fields),
  WID    = length(integer_to_list(MaxID+1)),
  MaxLen = lists:foldl(fun({field, AA, _}, I) -> max(I, length(atom_name(get_attr(name, AA),State))) end, 0, Fields),
  Filenm = add_variant_suffix("fix_fields.cpp", State),
  ok     = write_file(cpp, src, State, Filenm, [], [
            "#include \"util.hpp\"\n\n"
            "namespace {\n\n"
            "std::vector<Field> make_all_fields(FixVariant* fvar)\n"
            "{\n"
            "  assert(fvar);\n"
            "  auto vec = std::vector<Field>(", integer_to_list(MaxID+1), ", Field{});\n",
            lists:map(fun(I) ->
              % ID::integer(), FieldName::quoted_string(), FieldType::string(),
              % FieldType::atom(), Vals::list(), LenFldName::quoted_string()
              {ID, Name, Type, RawType, Vals, LenFldName, LenFldID} =
                case maps:find(I, FldMap) of
                  {ok, {N, 'NUMINGROUP' = T, _FldOrGrp, V}} ->
                    NN         = atom_to_list(N),
                    {Nm, LenF} = {NN, NN}, %% {group_name(NN), NN},
                    {I, Nm, atom_to_list(T), T, V, q(LenF), I};
                  {ok, {N, 'DATA' = T, _FldOrGrp, V}} ->
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
                  {ok, {N, T, _FldOrGrp, V}} ->
                    {I, atom_to_list(N), atom_to_list(T), T, V, "", 0};
                  error ->
                    {0, "nullptr", "UNDEFINED", undefined, [], "", 0}
                end,

              FirstZero = get(first_zero),

              if ID==0 ->
                (FirstZero == undefined) andalso put(first_zero, I),
                [];
              true ->
                erase(first_zero),
                LID = integer_to_list(ID),
                LE  = iif(Vals==[], 0, lists:max([length(CC) || {CC,  _} <- Vals])),
                LEn = length(integer_to_list(LE)),
                LL  = length(Vals),
                ME  = LE+2,
                MD  = iif(Vals==[], 0, lists:max([length(atom_name(caml_case(DD),State)) || {_,DD} <- Vals])+2),
                NVals = lists:zip(lists:seq(0, LL-1), Vals),
                [
                "  vec[", spad(I, WID), "] = Field{      //--- Tag# ", integer_to_list(I), " ", q(Name), "\n",
                "    fvar,\n"
                "    ", LID, ",\n"
                "    ", q(atom_name(Name,State)), ",\n"
                "    FieldType::", Type, ",\n"
                "    ", dtype(RawType, MapDT), ",\n"
                "    std::vector<FieldChoice>", iif(Vals==[], "(),", "{{"), "\n",
                lists:map(fun({II,{CC,DD}}) ->
                  io_lib:format(
                "      {.value = ~s, .descr = ~s, .atom = 0}, // ~w\n",
                                [qpad(CC,ME), qpad(atom_name(caml_case(DD),State), MD), II])
                end, NVals),
                iif(Vals==[], "", "    }},\n"),
                "    ", iif(LenFldName==[], "nullptr", LenFldName), ",\n"
                "    ", integer_to_list(LenFldID), ",\n",
                if Vals==[] ->
                  "    nullptr";
                true ->
                  "    [](const Field& f, ErlNifEnv* env, const char* code, int len) {"
                end,
                if
                  Vals==[] ->
                    "\n";
                  LE == 1 ->
                    ["\n"
                     "      if (!len) [[unlikely]] return am_", undef(State), ";\n"
                     "      switch (code[0]) {\n",
                     [io_lib:format("        case '~s': return f.value_atom(~s); // ~s\n",
                                    [CC,spad(integer_to_list(II),LEn),DD])
                      || {II,{CC,DD}} <- NVals],
                     "        default: return am_", undef(State), ";\n"
                     "      }\n"
                    ];
                  LE > 8 ->
                    ["\n"
                     "      auto   fc = f.value(std::string_view(code, len));\n",
                     "      return fc ? fc->get_atom(env) : am_", undef(State), ";\n"
                    ];
                  true ->
                    MML = integer_to_list(lists:max([length(to_char_str(CC)) || {CC,_} <- Vals])),
                    NW  = integer_to_list(length(integer_to_list(length(Vals)))),
                    ["\n"
                     "      auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);\n"
                     "      switch (hash) {\n",
                     [io_lib:format("        case ~"++MML++"s: return f.value_atom(~"++NW++"w); // ~s\n",
                        [iif(length(CC)==1, qname(CC), to_char_str(CC)),II,caml_case(DD)])
                      || {II,{CC,DD}} <- NVals],
                     "        default: return am_", undef(State), ";\n"
                     "      }\n"
                    ]
                end,
                iif(Vals==[], "", "    }\n"),
                "  };\n"
                ]
              end
            end, lists:seq(0, MaxID)),
            "\n"
            "  return vec;\n"
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
  Dsc = iif(State#state.variant=="", "", " " ++ State#state.variant),
  ANm = add_variant_suffix("fix", State),
  ok  = write_file(erlang, src, State, ANm++".app.src", [], [
    "{application, ", ANm, ",\n"
    "[\n"
    "  {description, \"", nvl(State#state.app_descr, ["FIX", Dsc, " library"]), "\"},\n"
    "  {vsn, \"1.0\"},\n"
    "  {registered, []},\n"
    "  {applications, [kernel, stdlib, ssl", iif(SFX=="", "", ", fix"), "]},\n"
    "  {env, [{config, []}]}\n"
    "]}.\n"
  ], false),
  FN2 = add_variant_suffix("fix_fields", State),
  ok  = write_file(erlang, src, State, FN2++".erl", [], [
    "-module(", FN2, ").\n"
    "-export([field/1, field_tag/1, encode_msg/2]).\n\n"
    "-import(fix_util, [try_encode_val/3, try_encode_group/3, encode_tagval/2]).\n\n",
    lists:map(fun({ID, {Name, Type, _FldOrGrp, Vals}}) ->
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
          ["try_encode_group",string:pad(" ",MID-2),"(?MODULE,", string:pad(integer_to_list(ID), MID, leading), ", V)"];
        Vals==[] ->
          ["try_encode_val",string:pad(" ",MID),"(", SS, ", V)"];
        true ->
          ["encode_fld_val",string:pad(integer_to_list(ID),MID),"(", SS, ", V)"]
       end,
       " end};\n"
      ]
     end || {ID, {Name, Type, _FldOrGrp, Vals}} <- FldList],
    "field_tag(_) -> erlang:error(badarg).\n"
    "\n"
    "-spec encode_msg(binary() | proplists:proplist(), binary()) -> iolist().\n"
    "encode_msg(Body0, FixVerStr) ->\n"
    "  Body1   = encode_msg(Body0),\n"
    "  BodyLen = iolist_size(Body1),\n"
    "  Body2   = [\n"
    "    encode_field(", sq(atom_name('BeginString',State)), ", FixVerStr),\n"
    "    encode_field(", sq(atom_name('BodyLength', State)),  ",  BodyLen),\n"
    "    Body1\n"
    "  ],\n"
    "  Bin  = iolist_to_binary(Body2),\n"
    "  SumI = lists:sum([Char || <<Char>> <= Bin]) rem 256,\n"
    "  SumB = list_to_binary(io_lib:format(\"~3..0B\", [SumI])),\n"
    "  <<Bin/binary, \"10=\", SumB/binary, 1>>.\n"
    "\n"
    "-spec encode_msg(binary() | proplists:proplist()) -> iolist().\n"
    "encode_msg([{K,V}|T]) when is_atom(K) ->\n"
    "  [encode_field(K, V) | encode_msg(T)];\n"
    "encode_msg([{K,V,Tag,_Pos}|T]) when is_atom(K), is_integer(Tag) ->\n"
    "  [encode_field(K, V) | encode_msg(T)];\n"
    "encode_msg([]) ->\n"
    "  [];\n"
    "encode_msg(Packet) when is_binary(Packet) ->\n"
    "  Packet.\n\n"
    "encode_field(Tag, Val) when is_atom(Tag) ->\n"
    "  {_Num, _Type, Fun} = field_tag(Tag),\n"
    "  Fun(Val).\n\n",

    [begin
      Tp = maps:get(Type, MapDT),
      LE = lists:max([length(CC1) || {CC1,_} <- Vals]),
      ME = LE+2,
      IL = integer_to_list(ID),
      MD = lists:max([length(atom_name(caml_case(DD1),State)) || {_,DD1} <- Vals])+2,
      IsBool = Type == 'BOOLEAN' andalso [C || {C,_} <- Vals] == ["N","Y"],
      ["\ndecode_fld_val", IL, "(Val) ->\n",
       "  case Val of",
       if IsBool ->
         ["\n    <<\"N\">> -> false;"
          "\n    <<\"Y\">> -> true;"];
       true ->
         [begin
            MD = lists:max([length(atom_name(caml_case(DD1),State)) || {_,DD1} <- Vals])+2,
            io_lib:format("\n    <<~s>> -> ~s; %% ~w",
                          [qpad(CC,ME), string:pad(qname(caml_case(DD),State), MD), II])
          end || {II,{CC,DD}} <- lists:zip(lists:seq(0, length(Vals)-1), Vals)]
       end,
       "\n    ", string:pad("_", ME+4), " -> Val"
       "\n  end.\n\n",
       if Tp /= group ->
         if IsBool ->
           [["encode_fld_val", IL, "(ID,_T, ", C1, ") -> encode_tagval(ID, <<",q(V1),">>)", T1, $\n]
             || {C1, V1, T1} <- [{"false", "N", ";"}, {"true ", "Y", "."}]];
         true ->
           [[["encode_fld_val", IL, "(ID,_T, ", sqpad(atom_name(caml_case(DD),State), MD), ") -> encode_tagval(ID, <<",
            qpad(CC,ME), ">>);\n"] || {CC,DD} <- Vals],
           "encode_fld_val", IL, "(ID, T, ", string:pad("V",MD), ") -> try_encode_val(ID, T, V).\n"]
         end;
       true ->
         []
       end
      ]
    end || {ID, {_Name, Type, _FldOrTag, Vals}} <- lists:sort(FldList), Vals /= []],
    "\n"
  ]),
  FN3 = add_variant_suffix("fix", State, "codec"),
  ok  = write_file(erlang, src, State, FN3++".erl", [], [
    "%% @doc Interface module for the FIX ", State#state.variant, " variant\n\n"
    "-module(", FN3, ").\n"
    "-export([decode/2, decode/3, decode_msg/1, encode_msg/2, encode/3, split/2, split/3]).\n"
    "\n"
    "-include_lib(\"fix/include/fix.hrl\").\n"
    "\n"
    "-define(FIX_VARIANT,         ", nvl(State#state.variant, "default"), ").\n"
    "-define(FIX_DECODER_MODULE,  fix_decoder", SFX, ").\n"
    "-define(FIX_ENCODER_MODULE,  fix_fields", SFX, ").\n"
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
    "     {ok, Rest::binary(), {MatchedFldCount::integer(), Header::map(),\n"
    "       {MsgType::atom(), Msg::map()}, UnparsedFields::list()}}\n"
    "   | {more, non_neg_integer()}\n"
    "   | error.\n"
    "decode(Mode, Bin, Options) ->\n"
    "  fix_util:decode(Mode, ?FIX_DECODER_MODULE, ?FIX_VARIANT, Bin, Options).\n"
    "\n"
    "decode(Mode, Bin) ->\n"
    "  decode(Mode, Bin, []).\n"
    "\n"
    "%% Decodes a list of [{Key, Value}] pairs to a FIX message.\n"
    "%% Use after splitting the message with split/1\n"
    "decode_msg(Msg) when is_list(Msg) ->\n"
    "  ?FIX_DECODER_MODULE:decode_msg(Msg).\n"
    "\n"
    "split(Mode, Bin) -> split(Mode, Bin, []).\n\n"
    "-spec split(nif|native, binary(), [binary|full]) ->\n"
    "        [{atom(), binary()|{integer(),integer()}, any(), {integer(),integer()}}].\n"
    "split(Mode, Bin, Opts) ->\n"
    "  fix_util:split(Mode, ?FIX_VARIANT, Bin, Opts).\n"
    "\n"
    "-spec encode(nif|native, Header::map(), {atom(), map()}) -> binary().\n"
    "encode(Mode, Hdr =\n"
    "      #{\n"
    "        ", sq(atom_name('MsgSeqNum', State)), "    := SeqNum,\n"
    "        ", sq(atom_name('SenderCompID', State)), " := Sender,\n"
    "        ", sq(atom_name('TargetCompID', State)), " := Target\n"
    "      },\n"
    "      {MsgName, Msg} = _Msg)\n"
    "->\n"
    "  Hdr0    = maps:without([\n"
    "             ", sq(atom_name('BeginString',State)), ", "
                   , sq(atom_name('MsgSeqNum',  State)), ",\n"
    "             ", sq(atom_name('SenderCompID', State)), ", "
                   , sq(atom_name('TargetCompID', State)), ",\n"
    "             ", sq(atom_name('SendingTime',  State)), "], Hdr),\n"
    "  TStamp  = case maps:find(", sq(atom_name('SendingTime', State)), ", Hdr0) of\n"
    "              {ok, TS} -> TS;\n"
    "              error    -> fix_util:timestamp()\n"
    "            end,\n"
    "  Hdr1    = maps:to_list(Hdr0),\n"
    "  HdrFlds = [KV || KV = {_,V} <- Hdr1, V /= ", undef(State), "],\n"
    "  MsgFlds = [KV || KV = {_,V} <- maps:to_list(Msg), V /= ", undef(State), "],\n"
    "  Fields  = HdrFlds ++ MsgFlds,\n"
    "  Body    = [\n"
    "    {", sq(atom_name('MsgType',      State)), ", MsgName},\n"
    "    {", sq(atom_name('SenderCompID', State)), ", Sender},\n"
    "    {", sq(atom_name('TargetCompID', State)), ", Target},\n"
    "    {", sq(atom_name('MsgSeqNum',    State)), ", SeqNum},\n"
    "    {", sq(atom_name('SendingTime',  State)), ", TStamp}\n"
    "    | Fields],\n"
    "  encode_msg(Mode, Body).\n"
    "\n"
    "encode_msg(native, Body) ->\n"
    "  ?FIX_DECODER_MODULE:encode_msg(Body, ?FIX_BEGIN_STR);\n"
    "\n"
    "encode_msg(nif, Body0) ->\n"
    "  Body1   = encode_msg_nif(Body0),\n"
    "  BodyLen = iolist_size(Body1),\n"
    "  Bin     = iolist_to_binary([\n"
    "    fix_nif:encode_field_tagvalue(?FIX_VARIANT, ", sq(atom_name('BeginString',State)), ", ?FIX_BEGIN_STR),\n"
    "    fix_nif:encode_field_tagvalue(?FIX_VARIANT, ", sq(atom_name('BodyLength', State)), ",  BodyLen),\n"
    "    Body1,\n"
    "    fix_nif:encode_field_tagvalue(?FIX_VARIANT, ", sq(atom_name('CheckSum',   State)), ", <<\"000\">>)\n"
    "  ]),\n"
    "  fix_nif:update_checksum(Bin).\n"
    "\n"
    "-spec encode_msg_nif(binary() | proplists:proplist()) -> iolist().\n"
    "encode_msg_nif([{K,V}|T]) when is_atom(K) ->\n"
    "  [fix_nif:encode_field_tagvalue(?FIX_VARIANT, K, V) | encode_msg_nif(T)];\n"
    "encode_msg_nif([{K,V,Tag,_Pos}|T]) when is_atom(K), is_integer(Tag) ->\n"
    "  [fix_nif:encode_field_tagvalue(?FIX_VARIANT, K, V) | encode_msg_nif(T)];\n"
    "encode_msg_nif([]) ->\n"
    "  [];\n"
    "encode_msg_nif(Packet) when is_binary(Packet) ->\n"
    "  Packet.\n"
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
  {'MsgType', _, _, MsgTypes} = maps:get(35, FldMap),
  MTW  = lists:max([length(atom_name(Msg,State)) || {Msg, _Type, _Cat, _Fields} <- Messages]),
  FNm  = add_variant_suffix("fix_decoder", State),
  ok   = write_file(erlang, src, State, FNm ++ ".erl", [],
  [
    "-module(", FNm, ").\n"
    "-export([decode_msg/1, decode_msg/2, field/1]).\n\n",
    if SFX == "" ->
      ["-include(\"fix.hrl\").\n"
       "-include(\"", add_variant_suffix("fix_adm_msgs.hrl", State), "\").\n"
       "-include(\"", add_variant_suffix("fix_app_msgs.hrl", State), "\").\n"];
    true ->
      ["-include(\"", add_variant_suffix("fix.hrl", State), "\").\n"]
    end,
    "\n"
    "-define(MAP_SET(_R, _M, _F, _V), _R#_M{fields = (R#_M.fields)#{_F => _V}}).\n"
    "\n"
    "field(N) -> fix_fields", SFX, ":field(N).\n\n"
    "decode_msg(Msg) when is_list(Msg) ->\n"
    "  case decode_msg_header(Msg, #header{}, 0, []) of\n"
    "    {#header{fields = H = #{", qname(atom_name('MsgType',State)), " := MT}}, I, L} when I > 0 ->\n"
    "      {M, I1, U} = decode_msg(MT, L),\n"
    "      {I1, H, M, U};\n"
    "    _ ->\n"
    "      false\n"
    "  end.\n"
    "\n",
    lists:map(fun({Enum, _Descr}) ->
      case lists:keyfind(Enum, 2, Messages) of
        false ->
          [];
        {Msg, _Type, _Cat, _Fields} ->
          [
            "decode_msg(", sqpad(atom_name(Msg,State), MTW+2), ",L)", spad("", MTW),
            " -> ", ["decode_msg_", spad(Msg,MTW), "(L, #", sq(atom_name(Msg,State)), "{}, 0, []);\n"]
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
        MWDe = lists:max([length(atom_name(M,State)) || {M, _T, _C, _FF} <- Messages]),
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
              "decode_msg_", spad(MsgName, MWD), "([{", sqpad(atom_name(FName,State),MWDe), ", ", Val, ",_Tag,_Pos}|T], R, I, U) ->",
              Add,
              " decode_msg_", MsgName, "(", Var, ", ?MAP_SET(R, ",
                sq(iif(MsgName=="header", MsgName, atom_name(MsgName,State))), ", ",
                sqpad(atom_name(FName,State),MWDe), ", ", Res, "), I+1, U);\n"
            ]
          end, Flds),
          "decode_msg_", spad(MsgName, MWD), "([{K,V}|T], R, I, U) ", spad("", MFW+9), " -> decode_msg_", MsgName, "([{K,V,", undef(State), ",", undef(State), "}|T], R, I, U);\n",
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
  NameMap   = maps:fold(fun(ID, {Name, _Type, _FldOrGrp, _Vals}, A) -> A#{Name => ID} end, #{}, FldMap),
  MaxIDLen  = length(integer_to_list(maps:fold(fun(ID, _, A) -> max(ID,A) end, 0, FldMap))),
  Mandatory = proplists:get_value(mandatory, Cfg, #{}),
  Optional  = proplists:get_value(optional,  Cfg, #{}),

  %% Check mandatory message configs for invalid message names
  maps:foreach(fun(Msg, _) ->
    lists:keyfind(Msg, 1, Messages) == error
      andalso throw("Invalid message name ~w found in mandatory message config!")
  end, Mandatory),
  %% Check optional message configs for invalid message names
  maps:foreach(fun(Msg, _) ->
    lists:keyfind(Msg, 1, Messages) == error
      andalso throw("Invalid message name ~w found in optional message config!")
  end, Optional),

  %% Check mandatory message configs for invalid field names
  maps:foreach(fun(Msg, Flds) ->
    [maps:find(I, NameMap) == error
        andalso throw("Invalid mandatory field ~w found in configuration of msg ~w", [I, Msg])
      || I <- Flds]
  end, Mandatory),

  %% Check optional message configs for invalid field names
  maps:foreach(fun(Msg, Flds) ->
    [maps:find(I, NameMap) == error
        andalso throw("Invalid optional field ~w found in configuration of msg ~w", [I, Msg])
      || I <- Flds]
  end, Optional),

  [{header, _, _, HdrFlds} | Msgs] = AllMsgs =
    lists:map(fun({M,T,C,FF}) ->
      {M, T, C, split_req_and_opt_fields(FF, maps:get(M, Mandatory, []), maps:get(M, Optional, []))}
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
  AdminHdr = [create_rec(header, "", HdrFlds, State, MaxReqFldLen, MaxIDLen, Name2ID)],
  AdminOut = [create_rec(M, Tp, FF, State, MaxReqFldLen, MaxIDLen, Name2ID) || {M, Tp, admin, FF} <- Msgs],
  AppOut   = [create_rec(M, Tp, FF, State, MaxNLen, MaxIDLen, Name2ID)      || {M, Tp, app,   FF} <- Msgs],

  FixHRL = [
    "-include_lib(\"fix/include/fix.hrl\").\n",
    "-include(\"", add_variant_suffix("fix_adm_msgs.hrl", State), "\").\n",
    "-include(\"", add_variant_suffix("fix_app_msgs.hrl", State), "\").\n"
    "\n" |
    AdminHdr
  ],

  SFX /= "" andalso
    (ok=write_file(erlang, inc, State, add_variant_suffix("fix.hrl", State), ["%% Common include for FIX ", State#state.variant, " variant\n"], FixHRL)),
  ok  = write_file(erlang, inc, State, add_variant_suffix("fix_adm_msgs.hrl", State), ["%% Administrative FIX messages\n"], AdminOut),
  ok  = write_file(erlang, inc, State, add_variant_suffix("fix_app_msgs.hrl", State), ["%% Application FIX messages\n"],    AppOut),
  FN4 = add_variant_suffix("fix_groups", State),
  ok  = write_file(erlang, src, State, FN4++".erl", ["%% Metadata about FIX groups\n"],
        [
          "-module(", FN4, ").\n\n",
          "-include(\"", add_variant_suffix("fix.hrl", State), "\").\n"
          "\n"
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
            ["decode_group(", MN, ", ", GA, ", L) -> decode_", atom_to_list(Msg), "_", atom_to_list(group_name(G)), "(L, ", undef(State), ", #group{name=", sq(G), "});\n"]
          end, AllGrps),
          "decode_group(_, _, _) -> false.\n\n",
          lists:map(fun({Msg, G, FF}) ->
            GN = atom_to_list(group_name(G)),
            FN = atom_to_list(Msg) ++ "_" ++ GN,
            ML = lists:max([length(atom_to_list(F)) || {F, _ID, _Req, _IsGrp} <- FF])+2,
            [
              "%% Parse Group: ", GN, " in message ", atom_to_list(Msg), "\n",
              "decode_",  FN, "(L, R) -> decode_", FN, "(L, ", undef(State), ", R).\n",
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
          "def(", undef(State), ",N) -> N;\n"
          "def(Other,   _N) -> Other.\n"
        ]),
  ok.

parse(["-f", Xml | T], S) -> parse(T, S#state{file     = Xml});
parse(["-o", Out | T], S) -> parse(T, S#state{outdir   = Out});
parse(["-c", Cfg | T], S) -> parse(T, S#state{config   = Cfg});
parse(["-cr", CR | T], S) -> parse(T, S#state{copyrt   =  CR});
parse(["-ad", AD | T], S) -> parse(T, S#state{app_descr=  AD});
parse(["-vsn", V | T], S) -> parse(T, S#state{version  =   V});
parse(["-vars",V | T], S) -> parse(T, S#state{variants =   V});
parse(["-sfx", V | T], S) -> parse(T, S#state{file_sfx =   V});
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
    "     -ad AppDescr        - Application description added to the app.src file\n"
    "     -e                  - Use Elixir atom prefixing\n"
    "     -ge                 - Generate Erlang code only\n"
    "     -gc                 - Generate C++ code only\n"
    "     -sfx  FileSfx       - Suffix to add to generated filenames (default: Variant)\n"
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

read_file(#state{file = Xml, schema = Schema, config = Config, debug = Debug}) ->
  InclMsgs   = proplists:get_value(include_app_messages, Config, []),
  Debug > 0    andalso io:format(standard_error, "InclMsgs config: ~p\n", [InclMsgs]),
  % Read file using the Schema to format value types
  {fix,AA,L} = xmltree:file(Xml, Schema),
  MajorVsn   = get_attr(major, AA),
  MinorVsn   = get_attr(minor, AA),
  FixVsn     = str(io_lib:format("FIX.~w.~w", [MajorVsn, MinorVsn])),
  Components = [C || {components, _, I} <- L, C <- I],
  [Header]   = [{header,  "", admin, expand_components(I, Components, header)}
                || {header,     _, I} <- L],
  [Trailer]  = [{trailer, "", admin, expand_components(I, Components, trailer)}
                || {trailer,    _, I} <- L],
  Fields     = [F || {fields,   _, I} <- L, F <- I],
  Messages   = [{M, T, C, expand_components(I, Components, M)}
                || {messages,   _, J} <- L,
                   {message,    A, I} <- J,
                   {msgtype,       T} <- A,
                   {name,          M} <- A,
                   C                  <- [get_attr(msgcat, A, app)],
                C==admin orelse InclMsgs == [] orelse lists:member(M, InclMsgs)],
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

  FldMap  = lists:foldl(fun({FldOrGrpTag, A, V} = F, M) ->
    Name  = get_attr(name,  A),
    ID    = get_attr(number,A),
    Type0 = get_attr(type,  A),
    Name /= undefined orelse throw({unknown_field_name, F}),
    is_integer(ID)    orelse throw({unknown_field_number, Name, ID}),
    Vals0 = lists:sort([{get_attr(enum, AX), get_attr(description, AX)} || {value, AX, _} <- V]),
    Len   = iif(Vals0==[], 0, lists:max([length(C) || {C,_} <- Vals0])),
    ValYN = Len==1 andalso [C || {C,_} <- Vals0] == ["N","Y"],
    {Type, Vals}  =
      if Type0=='BOOLEAN' orelse ((Type0=='CHAR') and ValYN) ->
        {'BOOLEAN', []};
      true ->
        {Type0, Vals0}
      end,
    M#{ID => {Name, Type, FldOrGrpTag, Vals}}
  end, #{}, Fields2),

  NameMap = maps:fold(fun(ID, {Name, _Type, _FldOrGrp, _Vals}, A) -> A#{Name => ID} end, #{}, FldMap),
  Name2ID = fun(N) -> maps:get(N, NameMap) end,
  AllGrps = get_all_groups([Header | Messages], Name2ID, []),

  %% Update group tags. In contrast to FIX4.4, FIX4.2 does not label them as
  %% the 'NUMINGROUP' type but instead defines them as 'INT'.
  FldMap2 = lists:foldl(fun({_, F, _}, M) ->
    {ok, ID} = maps:find(F, NameMap),
    %% E.g. {'NoAllocs','INT',field,[]}
    {ok, {Name1, _Type, _Field, Vals1}} = maps:find(ID, FldMap),
    M#{ID => {Name1, 'NUMINGROUP', group, Vals1}}
  end, FldMap, AllGrps),

  %io:format("AllGrps: ~p\n",   [AllGrps]),
  %io:format("MsgTypes:\n~p\n", [MsgTypes]),
  {FixVsn, Header, Trailer, Messages, AllGrps, Fields2, FldMap2}.

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

split_req_and_opt_fields(Fields, [], []) ->
  split_req_and_opt_fields(Fields);
split_req_and_opt_fields(Fields, MandFields, OptFields) ->
  FF = lists:map(fun({FldOrGrp, A, V}) ->
    Name  = get_attr(name,A),
    IsOpt = lists:member(Name, OptFields),
    IsReq = lists:member(Name, MandFields),
    Req   = case {IsOpt, IsReq} of
              {true, _} -> false;
              {_, true} -> add;
              _         -> get_attr(required,A)
            end,
    {FldOrGrp, lists:keystore(required, 1, A, {required, Req}), V}
  end, Fields),
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
                          false -> [" => ", undef(State)]
                        end),
              " %% Tag# ", string:pad(ID, MaxIDLen, leading),
              iif(IsGrp," (GroupLen: "++N++")", ""), "\n"]
             || {Idx, {N,IsGrp,ID}} <- lists:zip(lists:seq(1,Len), IDNames)],
  [
    iif(MsgTp=="" orelse MsgTp==group, "", "%% Message type: \""++MsgTp++"\"\n"),
    "-record(", qname(iif(Name=='header', atom_to_list(Name), atom_name(Name, State))), ", {\n"
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

spad(S, I) when is_atom(S)    -> string:pad(atom_to_list(S), I);
spad(S, I) when is_list(S)    -> string:pad(S, I);
spad(S, I) when is_integer(S) -> string:pad(integer_to_list(S), I, leading).

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

%undef(#state{elixir = true}) -> "nil";
undef(_)                     -> "nil".

qname(N, #state{} = S) ->
  "'"++atom_name(N,S)++"'".

qname(N) when is_atom(N) -> "'"++atom_to_list(N)++"'";
qname(N) when is_list(N) -> "'"++N++"'".

to_char_str(S) ->
  "CINT<"++string:join([qname([C]) || C <- S], ",")++">".

caml_case(S)   ->
  case lists:member($_, S) of
    true ->
      L0 = [titlecase(I) || I <- string:split(string:to_lower(S), "_", all)],
      put(len, 0),
      L = lists:takewhile(fun(I) -> N = get(len) + length(I), put(len, N), N < 128 end, L0),
      string:join(L, "");
    false ->
      S
  end.

titlecase([])    -> [];
titlecase([H|T]) -> [upcase(H)|T].

upcase(C) when C >= $a, C =< $z -> C-32;
upcase(C)                       -> C.

center(Wid, S) when is_integer(Wid) ->
  SS = lists:flatten(S),
  string:copies(" ", (Wid - length(SS)) div 2) ++ SS.

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
        C, center(78, ["Copyright (c) ", Year, " ", Copyright, ". All rights reserved."]), "\n",
        sep(Type, 0)
      ];
    true ->
      []
    end,
    C, center(78, "*** This file is auto-generated by https://github.com/saleyn/fix ***"), "\n",
    C, center(78, "*** DON'T MODIFY BY HAND!!! ***"), "\n",
    sep(Type, 0), "\n"
  ].

comment(cpp)    -> "//";
comment(erlang) -> "%%".

abort(Fmt, Opts / []) ->
  io:format(standard_error, lists:flatten(["ERROR: ", Fmt, "\n"]), Opts),
  halt(1).

get_fix_variant(#state{variant=""}) -> "default";
get_fix_variant(#state{variant=V})  -> V.

add_variant_suffix(File, #state{variant=V}, Def) when V==""; V=="default" ->
  File ++ "_" ++ Def;
add_variant_suffix(File, State, _Def) ->
  add_variant_suffix(File, State).

add_variant_suffix(File, #state{variant=V}) when V==""; V=="default" ->
  File;
add_variant_suffix(File, #state{file_sfx=V}) when V /= undefined, V /= "" ->
  add_variant_suffix(File, "_"++V);
add_variant_suffix(File, #state{var_sfx=Sfx}) ->
  add_variant_suffix(File, Sfx);
add_variant_suffix(File, Sfx) when is_list(Sfx) ->
  Ext  = filename:extension(File),
  Base = filename:basename(File, Ext),
  Base ++ Sfx ++ Ext.

write_file(Type, SrcOrInc, State, File, Header, Data) ->
  write_file(Type, SrcOrInc, State, File, Header, Data, true).
write_file(Type, SrcOrInc, #state{outdir=Cwd, gen=Gen} = State, File, Header,
           Data, Overwrite) ->
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
      case {Overwrite, filelib:is_regular(F)} of
        {false, true} ->
          io:format("Skipping file: ~s\n", [F]);
        _             ->
          io:format("Writing file:  ~s\n", [F]),
          ok   = file:write_file(F, Out)
        end;
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
      io:format("Writing file:  ~s\n", [DstFile]),
      {ok,F}  = file:read_file(SrcFile),
      Data    = re:replace(F, "INC_DIR\\s*:=[^\n]+", "INC_DIR := -I" ++ IncDir ++ "\n",
                           [{return, binary}]),
      ok      = file:write_file(Makefile, Data)
  end.
