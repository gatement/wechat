-module(wechat_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(wechat).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    wechat_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Helper functions
%% ===================================================================

start_cowboy() ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/callback", wechat_handler_callback, []},
			{"/[...]", wechat_handler_page, []}
		]}
	]),
    {ok, Port} = application:get_env(wechat, http_port),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]).
