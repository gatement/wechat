-module(wechat_http_handler).
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
        {<<"GET">>, [<<"wechat">>, <<"callback">>]} ->
            handle_wechat_callback(Req2);
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

handle_wechat_callback(Req) ->
    {SignatureBin, Req2} = cowboy_req:qs_val(<<"signature">>, Req),
    {TimestampBin, Req3} = cowboy_req:qs_val(<<"timestamp">>, Req2),
    {NonceBin, Req4} = cowboy_req:qs_val(<<"nonce">>, Req3),
    {EchostrBin, Req5} = cowboy_req:qs_val(<<"echostr">>, Req4),
    %?LOG_INFO("signature=~p, ts=~p, nonce=~p, echostr=~p~n", [SignatureBin, TimestampBin, NonceBin, EchostrBin]),
    case verify_signature(SignatureBin, TimestampBin, NonceBin) of
        true ->
            Res = EchostrBin,
            cowboy_req:reply(200, [], Res, Req5);
        false ->
            Res = <<"signature error.">>,
            cowboy_req:reply(400, [], Res, Req5)
    end.


handle_other(Req) ->
    cowboy_req:reply(404, [], <<"page not found.">>, Req).

%% ===================================================================
%% Helper functions
%% ===================================================================

verify_signature(SignatureBin, TimestampBin, NonceBin) ->
    {ok, Token} = application:get_env(wechat, wechat_token),
    Sorted = lists:sort([Token, binary_to_list(TimestampBin), binary_to_list(NonceBin)]),
    SignatureBin2 = crypto:hash(sha, string:join(Sorted, "")),
    SignatureBin =:= SignatureBin2.

%% ===================================================================
%% Eunit Tests
%% ===================================================================

-ifdef(TEST).

set_loglevel_test_() ->
    {foreach,
        fun() ->
                error_logger:tty(true),
                
                application:load(wechat),
                application:set_env(wechat, http_port, 8080),
                application:set_env(wechat, wechat_token, "123456"),
                
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
                        {ok,{{"HTTP/1.1",404,"Not Found"}, _Headers, "page not found."}} 
                            = httpc:request("http://localhost:8080/"),
                        {ok,{{"HTTP/1.1",404,"Not Found"}, _Headers, "page not found."}} 
                            = httpc:request("http://localhost:8080/abc"),
                        ok
                end
            },
            {"GET: wechat callback",
                fun() ->
                        %?assertEqual(info, info),
                        %{ok,{{"HTTP/1.1",200,"OK"}, _Headers, "ok"}} 
                        Res  = httpc:request("http://localhost:8080/wechat/callback?signature=c372d4ab7a2be1f20a7292e87bfc74925ecd687f&echostr=6980454302888222583&timestamp=1450565495&nonce=1182489064"),
                        ct:pal("res: ~p~n", [Res]),
                        ok
                end
            }
        ]
    }.

-endif.
