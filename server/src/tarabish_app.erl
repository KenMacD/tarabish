-module(tarabish_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    web:start(),
    tarabish_sup:start_link().

stop(_) ->
    ok.
