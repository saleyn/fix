%% This file implements a parameterized module for a given FIX variant

-include_lib("pmod_transform/include/pmod.hrl").

-module(fix_variant, [Variant]).

-export([split/1, split/2, tag_to_field/1, field_to_tag/1]).
-export([decode_field_value/2, encode_field_value/2, list_field_values/1]).

split(Bin) ->
  fix_nif:split(Variant, Bin).

-spec split(binary(), Options::[binary | full]) ->
  fix_nif:decoded_message().
split(Bin, Opts) ->
  fix_nif:split(Variant, Bin, Opts).

tag_to_field(Field) ->
  fix_nif:tag_to_field(Variant, Field).

field_to_tag(Field) ->
  fix_nif:field_to_tag(Variant, Field).

encode_field_value(Field, Value) ->
  fix_nif:encode_field_value(Variant, Field, Value).

decode_field_value(Field, Value) ->
  fix_nif:decode_field_value(Variant, Field, Value).

list_field_values(Field) ->
  fix_nif:list_field_values(Variant, Field).
