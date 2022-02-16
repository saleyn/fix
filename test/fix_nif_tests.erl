-module(fix_nif_tests).

-include_lib("eunit/include/eunit.hrl").


split_1_test() ->
  ?assertEqual([{msg_type,heartbeat}], fix_nif:split(<<"35=0",1>>)).


split_2_test() ->
  List = fix_nif:split(fix_tests:sample_md()),
  ?assertMatch([{begin_string, <<"FIX.4.4">>},{body_length,1084},{msg_type,market_data_snapshot_full_refresh},{msg_seq_num,2}|_], List).

split_3_test() ->
  ?assertMatch([{begin_string,<<"FIX.4.4">>},{body_length,22},{msg_type,heartbeat},{signature,<<"A",1,"89=234">>},{check_sum,<<"999">>}],
  fix_nif:split(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).

split_float_test() ->
  ?assertMatch([{md_entry_px,218.87}], fix_nif:split(<<"270=218.87",1>>)).


split_4_test() ->
  ?assertEqual([{poss_dup_flag,false}], fix_nif:split(<<"43=N",1>>)).

split_5_test() ->
  ?assertEqual([{poss_dup_flag,false},{sending_time,<<"20120502-13:08:35">>}], fix_nif:split(<<"43=N",1,"52=20120502-13:08:35",1>>)).

split_6_test() ->
  ?assertEqual([{order_qty,-287},{ord_status,pendingnew}], fix_nif:split(<<"38=-287",1, "39=A",1>>)).

dont_fail_on_unknown_code_test() ->
  ?assertEqual(10001, fix_nif:tag_to_field(10001)),
  ?assertEqual([{10001,<<"Y">>}], fix_nif:split(<<"10001=Y",1>>)).

good_field_names_test() ->
  Pairs = [{N,fix_parser:tag_to_field(list_to_binary(integer_to_list(N))), fix_nif:tag_to_field(N)} || N <- lists:seq(1, 956)],
  % SplitterPairs = [{Number, } || {Number, _Name} <- Pairs],
  % ?assertEqual(Pairs, SplitterPairs).
  [?assertEqual({Number,Field1}, {Number,Field2}) || {Number, Field1, Field2} <- Pairs],
  ok.
