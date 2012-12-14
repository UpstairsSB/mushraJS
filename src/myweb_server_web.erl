%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for myweb_server.

-module(myweb_server_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                	"result" ->
						Query = Req:parse_qs(),
						TestId = list_to_binary(proplists:get_value("t", Query)),
						{ok, HTMLOutput} = result_dtl:render([{testId, TestId}]),
						%%io:format("~p~n", [HTMLOutput]),
						Req:ok({"text/html", [], HTMLOutput});
                	"info" ->
						Query = Req:parse_qs(),
						TestId = list_to_binary(proplists:get_value("t", Query)),
                		Result = mushra:read_all_test_info(TestId),
						Struct2 = {struct, [{<<"result">>, {array, 
							Result
						}}]}, 
						DataOut = mochijson2:encode(Struct2),
						Req:ok({"application/json", [], [DataOut]});
                	"show" ->
						Query = Req:parse_qs(),
						TestId = list_to_binary(proplists:get_value("t", Query)),
                		Result = mushra:read_all(TestId),
						%%Struct = {struct, [{<<"result">>, {array, 
						%%	[{array, [mushra, <<"name">>, <<"reference">>, 100, 80, 1354245010135847]}, 
						%%	 {array, [mushra, <<"name">>, <<"reference">>, 100, 80, 1354245010135847]}]
						%%}}]}, 
						%%io:format("~nTable: ~p~n", [Table]),
						Struct2 = {struct, [{<<"result">>, {array, 
							Result
						}}]}, 
						DataOut = mochijson2:encode(Struct2),
						Req:ok({"application/json", [], [DataOut]});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
					"log" ->
						Data = Req:parse_post(),
						%%io:format("~nData: ~p~n", [Data]),
						Json = proplists:get_value("json", Data),
						%%io:format("~nJson: ~p~n", [Json]),
						Struct = mochijson2:decode(Json),
						io:format("~nStruct : ~p~n", [Struct]),
						Result = mushra:insert(Struct),
						JsonResult = {struct, [{ret, Result}]},
						DataOut = mochijson2:encode(JsonResult),
						%%io:format("~ndataOut: ~p~n", [binary_to_list(iolist_to_binary(DataOut))]),
						Req:ok({"application/json", [], [DataOut]}); 
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
