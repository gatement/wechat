-module(wechat_handler_page).
-include("wechat.hrl").

-export([init/3,
         handle/2,
         terminate/3
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API functions
%% ===================================================================

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {PathInfo, Req2} = cowboy_req:path_info(Req1),
    {ok, Req3} = case {Method, PathInfo} of
        _ ->
            ?LOG_INFO("got other req: method=~p, path=~p~n", [Method, PathInfo]),
            handle_other(Req2)
    end,
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% ===================================================================
%% Handler functions
%% ===================================================================

handle_other(Req) ->
    cowboy_req:reply(404, [], <<"page not found.">>, Req).

%% ===================================================================
%% Helper functions
%% ===================================================================



%% ===================================================================
%% Eunit Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->
    {foreach,
        fun() ->
                error_logger:tty(false),
                
                application:load(wechat),
                application:set_env(wechat, http_port, 8080),
                
                inets:start(),
                application:start(crypto),
                application:start(ranch),
                application:start(cowlib),
                application:start(cowboy),
                application:start(wechat)
        end,
        fun(_) ->
                inets:stop(),
                application:stop(wechat),
                application:stop(cowboy),
                application:stop(cowlib),
                application:stop(ranch),
                application:stop(crypto),
                error_logger:tty(true)
        end,
        [
            {"GET: page not found",
                fun() ->
                         {ok, {{"HTTP/1.1",404,"Not Found"}, _, "page not found."}} = httpc:request("http://localhost:8080/"),
                        ok
                end
            }
        ]
    }.

-endif.
