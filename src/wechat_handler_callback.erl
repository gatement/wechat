-module(wechat_handler_callback).
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
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = case Method of
        <<"GET">> ->
            handle_init(Req2);
        _ ->
            ?LOG_INFO("got other req2: method=~p~n", [Method]),
            handle_other(Req2)
    end,
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% ===================================================================
%% Handler functions
%% ===================================================================

handle_init(Req) ->
    {SignatureBin, Req2} = cowboy_req:qs_val(<<"signature">>, Req),
    {TimestampBin, Req3} = cowboy_req:qs_val(<<"timestamp">>, Req2),
    {NonceBin, Req4} = cowboy_req:qs_val(<<"nonce">>, Req3),
    {EchostrBin, Req5} = cowboy_req:qs_val(<<"echostr">>, Req4),
    ?LOG_INFO("signature=~p, ts=~p, nonce=~p, echostr=~p~n", [SignatureBin, TimestampBin, NonceBin, EchostrBin]),
    case verify_signature(SignatureBin, TimestampBin, NonceBin) of
        true ->
            Res = EchostrBin,
            ?LOG_INFO("verify signature succeeded.~n", []),
            cowboy_req:reply(200, [], Res, Req5);
        false ->
            ?LOG_INFO("verify signature fauled.~n", []),
            Res = <<"signature error.">>,
            cowboy_req:reply(400, [], Res, Req5)
    end.


handle_other(Req) ->
    cowboy_req:reply(404, [], <<"method is not allowed.">>, Req).

%% ===================================================================
%% Helper functions
%% ===================================================================

verify_signature(SignatureBin, TimestampBin, NonceBin) ->
    {ok, Token} = application:get_env(wechat, wechat_token),
    Sorted = lists:sort([Token, binary_to_list(TimestampBin), binary_to_list(NonceBin)]),
    SignatureBin2 = crypto:hash(sha, string:join(Sorted, "")),
    SignatureBin3 = list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X:8>> <= SignatureBin2])),
    ?LOG_INFO("signature2=~p, token=~p, sorted=~p~n", [SignatureBin3, Token, Sorted]),
    SignatureBin =:= SignatureBin3.

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
            {"GET: wechat callback",
                fun() ->
                        {ok, {{"HTTP/1.1",200,"OK"}, _, "1173434274695593183"}} = httpc:request("http://localhost:8080/callback?signature=88e6081f52fdf5e5c6eb13a1553fa5dfb16909fd&echostr=1173434274695593183&timestamp=1450686621&nonce=2095644193"),
                        {ok,{{"HTTP/1.1",400,"Bad Request"}, _, "signature error."}} = httpc:request("http://localhost:8080/callback?signature=88e6081f52fdf5e5c6eb13a1553fa5dfb16909fd&echostr=1173434274695593183&timestamp=1450686622&nonce=2095644193"),
                        ok
                end
            },
            {"DELETE: method is not allowed",
                fun() ->
                        {ok, {{"HTTP/1.1",404,"Not Found"}, _, "method is not allowed."}} = httpc:request(delete, {"http://localhost:8080/callback", []}, [], []),
                        ok
                end
            }
        ]
    }.

-endif.
