%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc myweb_server.

-module(myweb_server).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the myweb_server server.
start() ->
    myweb_server_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(myweb_server).


%% @spec stop() -> ok
%% @doc Stop the myweb_server server.
stop() ->
    Res = application:stop(myweb_server),
    application:stop(mnesia),
    ensure_started(crypto),
    Res.
