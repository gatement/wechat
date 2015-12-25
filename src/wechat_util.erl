-module(wechat_util).

%% API
-export([timestamp/0,
         timestamp_string/0,
         build_xml_text/1
        ]).

%% ===================================================================
%% API functions
%% ===================================================================

timestamp() ->
    {A, B, _} = erlang:now(),
    A * 1000000 + B.

timestamp_string() ->
    integer_to_list(?MODULE:timestamp()).

build_xml_text(Content) ->
    io_lib:format("<![CDATA[~s]]>", [Content]).

%% ===================================================================
%% Helper functions
%% ===================================================================


