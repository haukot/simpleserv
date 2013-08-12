-module(my_http_server).

%% Application callbacks
-export([start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(my_http_server).

stop() ->
    application:stop(my_http_server).