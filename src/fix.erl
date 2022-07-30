%% @author Max Lapshin <max@maxidoors.ru>
%% @copyright 2012 Max Lapshin
%% @doc Main module for fix usage.
%%
-module(fix).

-export([split/2, split/3, tag_to_field/2, field_to_tag/2]).
-export([decode_field_value/3, encode_field/3, encode_fields/2, list_field_values/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([sample_fix/0]).
-endif.

split(FixVariant, Bin) ->
  fix_nif:split(FixVariant, Bin).

-spec split(atom(), binary(), Options::[binary | full | {delim,char()}]) ->
  fix_nif:split_message().
split(FixVariant, Bin, Opts) ->
  fix_nif:split(FixVariant, Bin, Opts).

tag_to_field(FixVariant, Field) ->
  fix_nif:tag_to_field(FixVariant, Field).

field_to_tag(FixVariant, Field) ->
  fix_nif:field_to_tag(FixVariant, Field).

-spec encode_field(atom(), atom()|binary()|string()|integer(), atom()) -> binary().
encode_field(FixVariant, Field, Value) ->
  fix_nif:encode_field(FixVariant, Field, Value).

-spec encode_fields(atom(), [{atom()|binary()|string()|integer(), atom()|binary()}]) -> binary().
encode_fields(FixVariant, Fields) when is_list(Fields) ->
  fix_nif:encode_fields(FixVariant, Fields).

decode_field_value(FixVariant, Field, Value) ->
  fix_nif:decode_field_value(FixVariant, Field, Value).

list_field_values(FixVariant, Field) ->
  fix_nif:list_field_values(FixVariant, Field).


-ifdef(TEST).

sample_fix() ->
  fix_util:undump(
    <<"35=W|34=3|52=20170824-13:21:05.423287|55=ABC|262=42|268=2|269=0|"
      "270=18.920|271=10|269=1|270=19.120|271=140|">>).

-endif.
