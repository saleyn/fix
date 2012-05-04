-module(fix_splitter).

-on_load(init_nif/0).
-include("log.hrl").

-export([split/1, bench/0]).

init_nif() ->
  Path = filename:dirname(code:which(?MODULE)) ++ "/../priv",
  Load = erlang:load_nif(Path ++ "/fix_splitter", 0),
  case Load of
    ok -> ok;
    {error, {Reason,Text}} -> io:format("Load fix_splitter failed. ~p:~p~n", [Reason, Text])
  end,
  ok.

split(_Binary) ->
  erlang:error(not_implemented).


decode_fields_erl(Message) ->
  [begin
    [K,V] = binary:split(Field, <<"=">>),
    Tag = fix_parser:field_by_number(K),
    {Tag, fix_parser:decode_typed_field(Tag, V)}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0].

bench() ->
  ok.
