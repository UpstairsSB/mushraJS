%% @author Mochi Media <dev@mochimedia.com>
%% @copyright myweb_server Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the myweb_server application.

-module(myweb_server_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for myweb_server.
start(_Type, _StartArgs) ->
    myweb_server_deps:ensure(),
    myweb_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for myweb_server.
stop(_State) ->
    ok.
