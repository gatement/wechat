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
    {ok, Req3} = case verify_signature(Req) of
        false ->
            ?LOG_INFO("verify signature fauled.~n", []),
            Res = <<"signature error.">>,
            cowboy_req:reply(400, [], Res, Req);
        true ->
            {Method, Req2} = cowboy_req:method(Req),
            case Method of
                <<"GET">> ->
                    handle_init(Req2);
                <<"POST">> ->
                    handle_post(Req2);
                _ ->
                    ?LOG_INFO("got other req: method=~p~n", [Method]),
                    handle_other(Req2)
            end
    end,

    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% ===================================================================
%% Handler functions
%% ===================================================================

handle_init(Req) ->
    {EchostrBin, Req2} = cowboy_req:qs_val(<<"echostr">>, Req),
    Res = EchostrBin,
    ?LOG_INFO("verify signature succeeded.~n", []),
    cowboy_req:reply(200, [], Res, Req2).

handle_post(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    ?LOG_INFO("debug, body=~p~n", [Body]),
    cowboy_req:reply(200, [], <<"">>, Req2).

handle_other(Req) ->
    cowboy_req:reply(404, [], <<"method is not allowed.">>, Req).

%% ===================================================================
%% Helper functions
%% ===================================================================

verify_signature(Req) ->
    {SignatureBin, _} = cowboy_req:qs_val(<<"signature">>, Req, <<>>),
    {TimestampBin, _} = cowboy_req:qs_val(<<"timestamp">>, Req, <<>>),
    {NonceBin, _} = cowboy_req:qs_val(<<"nonce">>, Req, <<>>),
    %?LOG_INFO("signature=~p, ts=~p, nonce=~p~n", [SignatureBin, TimestampBin, NonceBin]),
    
    {ok, Token} = application:get_env(wechat, wechat_token),
    Sorted = lists:sort([Token, binary_to_list(TimestampBin), binary_to_list(NonceBin)]),
    SignatureBin2 = crypto:hash(sha, string:join(Sorted, "")),
    SignatureBin3 = list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X:8>> <= SignatureBin2])),
    %?LOG_INFO("signature2=~p, token=~p, sorted=~p~n", [SignatureBin3, Token, Sorted]),
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
            {"verify signature",
                fun() ->
                    {ok,{{"HTTP/1.1",400,"Bad Request"}, _, "signature error."}} = httpc:request(delete, {"http://localhost:8080/callback", []}, [], []),

                    {ok,{{"HTTP/1.1",400,"Bad Request"}, _, "signature error."}} = httpc:request("http://localhost:8080/callback?signature=88e6081f52fdf5e5c6eb13a1553fa5dfb16909fd&echostr=1173434274695593183&timestamp=1450686622&nonce=2095644193"),

                    {ok, {{"HTTP/1.1",200,"OK"}, _, "1173434274695593183"}} = httpc:request("http://localhost:8080/callback?signature=88e6081f52fdf5e5c6eb13a1553fa5dfb16909fd&echostr=1173434274695593183&timestamp=1450686621&nonce=2095644193"),

                    ok
                end
            }
        ]
    }.

-endif.
