-module(mushra).
-compile([nowarn_unused_vars]).

-include("records.hrl").
-export([insert/1, read_all/1, read_all_test_info/1]).

insert_test(Id, Test) ->
	TestId = struct:get_value(<<"testId">>, Test),
	TestName = struct:get_value(<<"testName">>, Test),
	Reference = struct:get_value(<<"reference">>, Test),
	TestAnswer = struct:get_value(<<"testAnswer">>, Test),
	{Now1, Now2, Now3} = erlang:now(),
	Time = ((Now1 * 1000000) + Now2) * 1000000 + Now3,
	case mushradb:write({mushra, Id, TestId, TestName, Reference, TestAnswer, Time}) of
		{atomic, ok} ->
			{struct, [{<<"message">>, ok}]};	
		{aborted, Reson} ->
			{struct, [{<<"message">>, aborted, Reson}]}
	end.

foreach(Id, [H | T]) ->
	insert_test(Id, H),
	foreach(Id, T);
foreach(Id, []) ->
	ok.
	
insert(S) ->
	Id = struct:get_value(<<"id">>, S),
	Tests = struct:get_value(<<"tests">>, S),
	foreach(Id, Tests).
	
read_all(TestId) ->
	All = mushradb:read_all_test(mushra, TestId),
	%%io:format("~nread_all: ~p~n", [All]),
	make_json(All).

read_all_test_info(TestId) ->
	All = mushradb:read_all_test_info(test_info, TestId),
	%%io:format("~nread_all: ~p~n", [All]),
	make_json(All).

make_json(List) ->
	append_recursive(List, []).
	
append_recursive([], NewList) ->
	lists:reverse(NewList);
append_recursive([H|T], NewList) ->
	append_recursive(T, [{array, erlang:tuple_to_list(H)} | NewList]). 

