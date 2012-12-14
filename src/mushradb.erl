-module(mushradb).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).
-include("records.hrl").

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	
	mnesia:create_table(mushra, [{disc_copies, [node()]}, {type, bag}, {attributes, record_info(fields, mushra)}]),
	mnesia:create_table(expert, [{disc_copies, [node()]}, {type, bag}, {attributes, record_info(fields, expert)}]),
	mnesia:create_table(test_info, [{disc_copies, [node()]}, {type, bag}, {attributes, record_info(fields, test_info)}]).

insert_test() ->
	{Now1, Now2, Now3} = erlang:now(),
	Time = ((Now1 * 1000000) + Now2) * 1000000 + Now3,
	Rows = [{mushra, <<"test1@nhn.com">>, <<"No1">>, <<"testName1">>, 86, [100, 98, 97, 96, 95], Time},
		{mushra, <<"test2@nhn.com">>, <<"No1">>, <<"testName2">>, 66, [100, 98, 97, 96, 95], Time},
		{mushra, <<"test3@nhn.com">>, <<"No1">>, <<"testName2">>, 66, [100, 98, 97, 96, 95], Time},
		{mushra, <<"test4@nhn.com">>, <<"No1">>, <<"testName2">>, 99, [100, 98, 97, 96, 95], Time},
		{mushra, <<"test5@nhn.com">>, <<"No1">>, <<"testName2">>, 10, [100, 98, 97, 96, 95], Time},
		{expert, <<"test1@nhn.com">>, <<"testName1">>},
		{expert, <<"test4@nhn.com">>, <<"testName1">>},
		{expert, <<"test5@nhn.com">>, <<"testName1">>},
		{expert, <<"test2@nhn.com">>, <<"testName2">>},
		{test_info, <<"No1">>, <<"testName1">>, 0, <<"reference">>},
		{test_info, <<"No1">>, <<"testName1">>, 1, <<"ios3_to_win">>},
		{test_info, <<"No1">>, <<"testName1">>, 2, <<"ios4_to_win">>},
		{test_info, <<"No1">>, <<"testName1">>, 3, <<"mac_to_win">>},
		{test_info, <<"No1">>, <<"testName1">>, 4, <<"win_to_win">>},
		{test_info, <<"No1">>, <<"testName1">>, 5, <<"win_to_mac">>},
		{test_info, <<"mobileToPc">>, <<"mobileToPc">>, 0, <<"reference">>},
        {test_info, <<"mobileToPc">>, <<"mobileToPc">>, 1, <<"i4_to_mac">>},
        {test_info, <<"mobileToPc">>, <<"mobileToPc">>, 2, <<"mac_to_win">>},
        {test_info, <<"mobileToPc">>, <<"mobileToPc">>, 3, <<"s2_to_mac">>},
        {test_info, <<"mobileToPc">>, <<"mobileToPc">>, 4, <<"win_to_mac">>}
	],
	F = fun() ->
		foreach(fun write/1, Rows)
	end,
	mnesia:transaction(F).

write_test_info([H | T]) ->
	F = fun() ->
		mnesia:write(H)
	end,
	mnesia:transaction(F),
	write_test_info(T);
write_test_info([]) ->	
	ok.

write(Rec) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

do(Q) ->
	F = fun() ->
		qlc:e(Q)
	end,
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, Reson} ->
			Reson
	end.
	
read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	do(Q).

read_all_test(Table, TestId) ->
	Q = qlc:sort(qlc:q([X || X <- mnesia:table(Table),
					X#mushra.test_id =:= TestId]),
					{order, fun(Row1, Row2) -> Row1#mushra.test_name < Row2#mushra.test_name end}),
	do(Q).

read_all_test_info(Table, TestId) ->
	Q = qlc:sort(qlc:q([X || X <- mnesia:table(Table),
					X#test_info.test_id =:= TestId]),
					{order, fun(Row1, Row2) -> Row1#test_info.question_index < Row2#test_info.question_index end}),
	do(Q).

expert_data(TestName) ->
	Q = qlc:q([X || X <- mnesia:table(mushra), Y <- mnesia:table(expert), 
					X#mushra.id =:= Y#expert.id, 
					X#mushra.test_name =:= Y#expert.test_name,
					X#mushra.test_name =:= TestName]),
	do(Q).

find_expert(TestName) ->
	Q = qlc:sort(qlc:q([X || X <- mnesia:table(mushra),
					X#mushra.test_name =:= TestName
					]),{order, fun(Row1, Row2) -> Row1#mushra.reference > Row2#mushra.reference end}),
	do(Q).
